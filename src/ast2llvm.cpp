#include "ast2llvm.h"
#include <vector>
#include <unordered_map>
#include <string>
#include <cassert>
#include <list>

using namespace std;
using namespace LLVMIR;

static unordered_map<string, FuncType> funcReturnMap;
static unordered_map<string, StructInfo> structInfoMap;
static unordered_map<string, Name_name *> globalVarMap;
static unordered_map<string, Temp_temp *> localVarMap;
static list<L_stm *> emit_irs;

LLVMIR::L_prog *ast2llvm(aA_program p)
{
    auto defs = ast2llvmProg_first(p);
    auto funcs = ast2llvmProg_second(p);
    vector<L_func *> funcs_block;
    for (const auto &f : funcs)
    {
        funcs_block.push_back(ast2llvmFuncBlock(f));
    }
    for (auto &f : funcs_block)
    {
        ast2llvm_moveAlloca(f);
    }
    return new L_prog(defs, funcs_block);
}

int ast2llvmRightVal_first(aA_rightVal r)
{
    if (r == nullptr)
    {
        return 0;
    }
    switch (r->kind)
    {
    case A_arithExprValKind:
    {
        return ast2llvmArithExpr_first(r->u.arithExpr);
        break;
    }
    case A_boolExprValKind:
    {
        return ast2llvmBoolExpr_first(r->u.boolExpr);
        break;
    }
    default:
        break;
    }
    return 0;
}

int ast2llvmBoolExpr_first(aA_boolExpr b)
{
    switch (b->kind)
    {
    case A_boolBiOpExprKind:
    {
        return ast2llvmBoolBiOpExpr_first(b->u.boolBiOpExpr);
        break;
    }
    case A_boolUnitKind:
    {
        return ast2llvmBoolUnit_first(b->u.boolUnit);
        break;
    }
    default:
        break;
    }
    return 0;
}

int ast2llvmBoolBiOpExpr_first(aA_boolBiOpExpr b)
{
    int l = ast2llvmBoolExpr_first(b->left);
    int r = ast2llvmBoolExpr_first(b->right);
    if (b->op == A_and)
    {
        return l && r;
    }
    else
    {
        return l || r;
    }
}

int ast2llvmBoolUOpExpr_first(aA_boolUOpExpr b)
{
    if (b->op == A_not)
    {
        return !ast2llvmBoolUnit_first(b->cond);
    }
    return 0;
}

int ast2llvmBoolUnit_first(aA_boolUnit b)
{
    switch (b->kind)
    {
    case A_comOpExprKind:
    {
        return ast2llvmComOpExpr_first(b->u.comExpr);
        break;
    }
    case A_boolExprKind:
    {
        return ast2llvmBoolExpr_first(b->u.boolExpr);
        break;
    }
    case A_boolUOpExprKind:
    {
        return ast2llvmBoolUOpExpr_first(b->u.boolUOpExpr);
        break;
    }
    default:
        break;
    }
    return 0;
}

int ast2llvmComOpExpr_first(aA_comExpr c)
{
    auto l = ast2llvmExprUnit_first(c->left);
    auto r = ast2llvmExprUnit_first(c->right);
    switch (c->op)
    {
    case A_lt:
    {
        return l < r;
        break;
    }
    case A_le:
    {
        return l <= r;
        break;
    }
    case A_gt:
    {
        return l > r;
        break;
    }
    case A_ge:
    {
        return l >= r;
        break;
    }
    case A_eq:
    {
        return l == r;
        break;
    }
    case A_ne:
    {
        return l != r;
        break;
    }
    default:
        break;
    }
    return 0;
}

int ast2llvmArithBiOpExpr_first(aA_arithBiOpExpr a)
{
    auto l = ast2llvmArithExpr_first(a->left);
    auto r = ast2llvmArithExpr_first(a->right);
    switch (a->op)
    {
    case A_add:
    {
        return l + r;
        break;
    }
    case A_sub:
    {
        return l - r;
        break;
    }
    case A_mul:
    {
        return l * r;
        break;
    }
    case A_div:
    {
        return l / r;
        break;
    }
    default:
        break;
    }
    return 0;
}

int ast2llvmArithUExpr_first(aA_arithUExpr a)
{
    if (a->op == A_neg)
    {
        return -ast2llvmExprUnit_first(a->expr);
    }
    return 0;
}

int ast2llvmArithExpr_first(aA_arithExpr a)
{
    switch (a->kind)
    {
    case A_arithBiOpExprKind:
    {
        return ast2llvmArithBiOpExpr_first(a->u.arithBiOpExpr);
        break;
    }
    case A_exprUnitKind:
    {
        return ast2llvmExprUnit_first(a->u.exprUnit);
        break;
    }
    default:
        assert(0);
        break;
    }
    return 0;
}

int ast2llvmExprUnit_first(aA_exprUnit e)
{
    if (e->kind == A_numExprKind)
    {
        return e->u.num;
    }
    else if (e->kind == A_arithExprKind)
    {
        return ast2llvmArithExpr_first(e->u.arithExpr);
    }
    else if (e->kind == A_arithUExprKind)
    {
        return ast2llvmArithUExpr_first(e->u.arithUExpr);
    }
    else
    {
        assert(0);
    }
    return 0;
}

std::vector<LLVMIR::L_def *> ast2llvmProg_first(aA_program p)
{
    vector<L_def *> defs;
    defs.push_back(L_Funcdecl("getch", vector<TempDef>(), FuncType(ReturnType::INT_TYPE)));
    defs.push_back(L_Funcdecl("getint", vector<TempDef>(), FuncType(ReturnType::INT_TYPE)));
    defs.push_back(L_Funcdecl("putch", vector<TempDef>{TempDef(TempType::INT_TEMP)}, FuncType(ReturnType::VOID_TYPE)));
    defs.push_back(L_Funcdecl("putint", vector<TempDef>{TempDef(TempType::INT_TEMP)}, FuncType(ReturnType::VOID_TYPE)));
    defs.push_back(L_Funcdecl("putarray", vector<TempDef>{TempDef(TempType::INT_TEMP), TempDef(TempType::INT_PTR, -1)}, FuncType(ReturnType::VOID_TYPE)));
    defs.push_back(L_Funcdecl("_sysy_starttime", vector<TempDef>{TempDef(TempType::INT_TEMP)}, FuncType(ReturnType::VOID_TYPE)));
    defs.push_back(L_Funcdecl("_sysy_stoptime", vector<TempDef>{TempDef(TempType::INT_TEMP)}, FuncType(ReturnType::VOID_TYPE)));
    for (const auto &v : p->programElements)
    {
        switch (v->kind)
        {
        case A_programNullStmtKind:
        {
            break;
        }
        case A_programVarDeclStmtKind:
        {
            if (v->u.varDeclStmt->kind == A_varDeclKind)
            {
                if (v->u.varDeclStmt->u.varDecl->kind == A_varDeclScalarKind)
                {
                    if (v->u.varDeclStmt->u.varDecl->u.declScalar->type->type == A_structTypeKind)
                    {
                        globalVarMap.emplace(*v->u.varDeclStmt->u.varDecl->u.declScalar->id,
                                             Name_newname_struct(Temp_newlabel_named(*v->u.varDeclStmt->u.varDecl->u.declScalar->id), *v->u.varDeclStmt->u.varDecl->u.declScalar->type->u.structType));
                        TempDef def(TempType::STRUCT_TEMP, 0, *v->u.varDeclStmt->u.varDecl->u.declScalar->type->u.structType);
                        defs.push_back(L_Globaldef(*v->u.varDeclStmt->u.varDecl->u.declScalar->id, def, vector<int>()));
                    }
                    else
                    {
                        globalVarMap.emplace(*v->u.varDeclStmt->u.varDecl->u.declScalar->id,
                                             Name_newname_int(Temp_newlabel_named(*v->u.varDeclStmt->u.varDecl->u.declScalar->id)));
                        TempDef def(TempType::INT_TEMP, 0);
                        defs.push_back(L_Globaldef(*v->u.varDeclStmt->u.varDecl->u.declScalar->id, def, vector<int>()));
                    }
                }
                else if (v->u.varDeclStmt->u.varDecl->kind == A_varDeclArrayKind)
                {
                    if (v->u.varDeclStmt->u.varDecl->u.declArray->type->type == A_structTypeKind)
                    {
                        globalVarMap.emplace(*v->u.varDeclStmt->u.varDecl->u.declArray->id,
                                             Name_newname_struct_ptr(Temp_newlabel_named(*v->u.varDeclStmt->u.varDecl->u.declArray->id), v->u.varDeclStmt->u.varDecl->u.declArray->len, *v->u.varDeclStmt->u.varDecl->u.declArray->type->u.structType));
                        TempDef def(TempType::STRUCT_PTR, v->u.varDeclStmt->u.varDecl->u.declArray->len, *v->u.varDeclStmt->u.varDecl->u.declArray->type->u.structType);
                        defs.push_back(L_Globaldef(*v->u.varDeclStmt->u.varDecl->u.declArray->id, def, vector<int>()));
                    }
                    else
                    {
                        globalVarMap.emplace(*v->u.varDeclStmt->u.varDecl->u.declArray->id,
                                             Name_newname_int_ptr(Temp_newlabel_named(*v->u.varDeclStmt->u.varDecl->u.declArray->id), v->u.varDeclStmt->u.varDecl->u.declArray->len));
                        TempDef def(TempType::INT_PTR, v->u.varDeclStmt->u.varDecl->u.declArray->len);
                        defs.push_back(L_Globaldef(*v->u.varDeclStmt->u.varDecl->u.declArray->id, def, vector<int>()));
                    }
                }
                else
                {
                    assert(0);
                }
            }
            else if (v->u.varDeclStmt->kind == A_varDefKind)
            {
                if (v->u.varDeclStmt->u.varDef->kind == A_varDefScalarKind)
                {
                    if (v->u.varDeclStmt->u.varDef->u.defScalar->type->type == A_structTypeKind)
                    {
                        globalVarMap.emplace(*v->u.varDeclStmt->u.varDef->u.defScalar->id,
                                             Name_newname_struct(Temp_newlabel_named(*v->u.varDeclStmt->u.varDef->u.defScalar->id), *v->u.varDeclStmt->u.varDef->u.defScalar->type->u.structType));
                        TempDef def(TempType::STRUCT_TEMP, 0, *v->u.varDeclStmt->u.varDef->u.defScalar->type->u.structType);
                        defs.push_back(L_Globaldef(*v->u.varDeclStmt->u.varDef->u.defScalar->id, def, vector<int>()));
                    }
                    else
                    {
                        globalVarMap.emplace(*v->u.varDeclStmt->u.varDef->u.defScalar->id,
                                             Name_newname_int(Temp_newlabel_named(*v->u.varDeclStmt->u.varDef->u.defScalar->id)));
                        TempDef def(TempType::INT_TEMP, 0);
                        vector<int> init;
                        init.push_back(ast2llvmRightVal_first(v->u.varDeclStmt->u.varDef->u.defScalar->val));
                        defs.push_back(L_Globaldef(*v->u.varDeclStmt->u.varDef->u.defScalar->id, def, init));
                    }
                }
                else if (v->u.varDeclStmt->u.varDef->kind == A_varDefArrayKind)
                {
                    if (v->u.varDeclStmt->u.varDef->u.defArray->type->type == A_structTypeKind)
                    {
                        globalVarMap.emplace(*v->u.varDeclStmt->u.varDef->u.defArray->id,
                                             Name_newname_struct_ptr(Temp_newlabel_named(*v->u.varDeclStmt->u.varDef->u.defArray->id), v->u.varDeclStmt->u.varDef->u.defArray->len, *v->u.varDeclStmt->u.varDef->u.defArray->type->u.structType));
                        TempDef def(TempType::STRUCT_PTR, v->u.varDeclStmt->u.varDef->u.defArray->len, *v->u.varDeclStmt->u.varDef->u.defArray->type->u.structType);
                        defs.push_back(L_Globaldef(*v->u.varDeclStmt->u.varDef->u.defArray->id, def, vector<int>()));
                    }
                    else
                    {
                        globalVarMap.emplace(*v->u.varDeclStmt->u.varDef->u.defArray->id,
                                             Name_newname_int_ptr(Temp_newlabel_named(*v->u.varDeclStmt->u.varDef->u.defArray->id), v->u.varDeclStmt->u.varDef->u.defArray->len));
                        TempDef def(TempType::INT_PTR, v->u.varDeclStmt->u.varDef->u.defArray->len);
                        vector<int> init;
                        for (auto &el : v->u.varDeclStmt->u.varDef->u.defArray->vals)
                        {
                            init.push_back(ast2llvmRightVal_first(el));
                        }
                        defs.push_back(L_Globaldef(*v->u.varDeclStmt->u.varDef->u.defArray->id, def, init));
                    }
                }
                else
                {
                    assert(0);
                }
            }
            else
            {
                assert(0);
            }
            break;
        }
        case A_programStructDefKind:
        {
            StructInfo si;
            int off = 0;
            vector<TempDef> members;
            for (const auto &decl : v->u.structDef->varDecls)
            {
                if (decl->kind == A_varDeclScalarKind)
                {
                    if (decl->u.declScalar->type->type == A_structTypeKind)
                    {
                        TempDef def(TempType::STRUCT_TEMP, 0, *decl->u.declScalar->type->u.structType);
                        MemberInfo info(off++, def);
                        si.memberinfos.emplace(*decl->u.declScalar->id, info);
                        members.push_back(def);
                    }
                    else
                    {
                        TempDef def(TempType::INT_TEMP, 0);
                        MemberInfo info(off++, def);
                        si.memberinfos.emplace(*decl->u.declScalar->id, info);
                        members.push_back(def);
                    }
                }
                else if (decl->kind == A_varDeclArrayKind)
                {
                    if (decl->u.declArray->type->type == A_structTypeKind)
                    {
                        TempDef def(TempType::STRUCT_PTR, decl->u.declArray->len, *decl->u.declArray->type->u.structType);
                        MemberInfo info(off++, def);
                        si.memberinfos.emplace(*decl->u.declArray->id, info);
                        members.push_back(def);
                    }
                    else
                    {
                        TempDef def(TempType::INT_PTR, decl->u.declArray->len);
                        MemberInfo info(off++, def);
                        si.memberinfos.emplace(*decl->u.declArray->id, info);
                        members.push_back(def);
                    }
                }
                else
                {
                    assert(0);
                }
            }
            structInfoMap.emplace(*v->u.structDef->id, std::move(si));
            defs.push_back(L_Structdef(*v->u.structDef->id, members));
            break;
        }
        case A_programFnDeclStmtKind:
        {
            FuncType type;
            if (v->u.fnDeclStmt->fnDecl->type == nullptr)
            {
                type.type = ReturnType::VOID_TYPE;
            }
            if (v->u.fnDeclStmt->fnDecl->type->type == A_nativeTypeKind)
            {
                type.type = ReturnType::INT_TYPE;
            }
            else if (v->u.fnDeclStmt->fnDecl->type->type == A_structTypeKind)
            {
                type.type = ReturnType::STRUCT_TYPE;
                type.structname = *v->u.fnDeclStmt->fnDecl->type->u.structType;
            }
            else
            {
                assert(0);
            }
            if (funcReturnMap.find(*v->u.fnDeclStmt->fnDecl->id) == funcReturnMap.end())
                funcReturnMap.emplace(*v->u.fnDeclStmt->fnDecl->id, std::move(type));
            vector<TempDef> args;
            for (const auto &decl : v->u.fnDeclStmt->fnDecl->paramDecl->varDecls)
            {
                if (decl->kind == A_varDeclScalarKind)
                {
                    if (decl->u.declScalar->type->type == A_structTypeKind)
                    {
                        TempDef def(TempType::STRUCT_PTR, 0, *decl->u.declScalar->type->u.structType);
                        args.push_back(def);
                    }
                    else
                    {
                        TempDef def(TempType::INT_TEMP, 0);
                        args.push_back(def);
                    }
                }
                else if (decl->kind == A_varDeclArrayKind)
                {
                    if (decl->u.declArray->type->type == A_structTypeKind)
                    {
                        TempDef def(TempType::STRUCT_PTR, -1, *decl->u.declArray->type->u.structType);
                        args.push_back(def);
                    }
                    else
                    {
                        TempDef def(TempType::INT_PTR, -1);
                        args.push_back(def);
                    }
                }
                else
                {
                    assert(0);
                }
            }
            defs.push_back(L_Funcdecl(*v->u.fnDeclStmt->fnDecl->id, args, type));
            break;
        }
        case A_programFnDefKind:
        {
            if (funcReturnMap.find(*v->u.fnDef->fnDecl->id) == funcReturnMap.end())
            {
                FuncType type;
                if (v->u.fnDef->fnDecl->type == nullptr)
                {
                    type.type = ReturnType::VOID_TYPE;
                }
                else if (v->u.fnDef->fnDecl->type->type == A_nativeTypeKind)
                {
                    type.type = ReturnType::INT_TYPE;
                }
                else if (v->u.fnDef->fnDecl->type->type == A_structTypeKind)
                {
                    type.type = ReturnType::STRUCT_TYPE;
                    type.structname = *v->u.fnDef->fnDecl->type->u.structType;
                }
                else
                {
                    assert(0);
                }
                funcReturnMap.emplace(*v->u.fnDef->fnDecl->id, std::move(type));
            }
            break;
        }
        default:
            assert(0);
            break;
        }
    }
    return defs;
}

// 第二次遍历程序ast
std::vector<Func_local *> ast2llvmProg_second(aA_program p)
{
    vector<Func_local *> funs;
    for (const auto &v : p->programElements)
    {
        switch (v->kind)
        {
        case A_programFnDefKind:
        {
            funs.push_back(ast2llvmFunc(v->u.fnDef));
            emit_irs.clear();
            localVarMap.clear();
            break;
        }
        default:
            break;
        }
    }
    return funs;
}

Func_local *ast2llvmFunc(aA_fnDef f)
{
    vector<Temp_temp *> args;
    Temp_temp *arg;
    bool has_intParam = false;
    // 生成函数名对应的标签
    string fun_name = *f->fnDecl->id;
    emit_irs.push_back(L_Label(Temp_newlabel_named(fun_name)));
    for (const auto &decl : f->fnDecl->paramDecl->varDecls)
    {
        if (decl->kind == A_varDeclScalarKind)
        {
            if (decl->u.declScalar->type->type == A_structTypeKind)
            {
                arg = Temp_newtemp_struct_ptr(0, *(decl->u.declScalar->type->u.structType));
                args.push_back(arg);
                localVarMap.emplace(*(decl->u.declScalar->id), arg);
            }
            else
            {
                arg = Temp_newtemp_int();
                args.push_back(arg);
                // 对于int类型的参数处理有不同
                Temp_temp *var = Temp_newtemp_int_ptr(0);
                AS_operand *dst = AS_Operand_Temp(var);
                emit_irs.push_back(L_Alloca(dst));
                // 将arg的值传给dst
                emit_irs.push_back(L_Store(AS_Operand_Temp(arg), dst));
                // 局部变量temp类型应该是int_ptr型
                localVarMap.emplace(*(decl->u.declScalar->id), var);
                has_intParam = true;
            }
        }
        else if (decl->kind == A_varDeclArrayKind)
        {
            if (decl->u.declArray->type->type == A_structTypeKind)
            {
                arg = Temp_newtemp_struct_ptr(-1, *(decl->u.declArray->type->u.structType));
                args.push_back(arg);
                localVarMap.emplace(*(decl->u.declArray->id), arg);
            }
            else
            {
                arg = Temp_newtemp_int_ptr(-1);
                args.push_back(arg);
                localVarMap.emplace(*(decl->u.declArray->id), arg);
            }
        }
        else
        {
            assert(0);
        }
    }
    // 有int型参数，需要标签跳转
    if (has_intParam)
    {
        Temp_label *l = Temp_newlabel();
        // 跳转到指定的label处
        emit_irs.push_back(L_Jump(l));
        emit_irs.push_back(L_Label(l));
    }
    // 对函数体中的每条指令处理
    for (const auto &stmt : f->stmts)
    {
        ast2llvmBlock(stmt, nullptr, nullptr);
    }
    // 对没有return语句的函数处理？？？
    if (emit_irs.back()->type != L_StmKind::T_RETURN)
    {
        if (funcReturnMap[*f->fnDecl->id].type == ReturnType::VOID_TYPE)
            emit_irs.push_back(L_Ret(nullptr));
        else
            emit_irs.push_back(L_Ret(AS_Operand_Const(0)));
    }
    return new Func_local(fun_name, funcReturnMap[fun_name], args, emit_irs);
}

void ast2llvmBlock(aA_codeBlockStmt b, Temp_label *con_label, Temp_label *bre_label)
{
    switch (b->kind)
    {
    case A_codeBlockStmtType::A_assignStmtKind:
    {
        AS_operand *r = ast2llvmRightVal(b->u.assignStmt->rightVal);
        AS_operand *l = ast2llvmLeftVal(b->u.assignStmt->leftVal);
        // 如果l是int类型使用L_Move
        if (l->kind == OperandKind::TEMP && l->u.TEMP->type == TempType::INT_TEMP)
        {
            emit_irs.push_back(L_Move(r, l));
            break;
        }
        //否则使用L_Store
        else
        {
            emit_irs.push_back(L_Store(r, l));
        }

        break;
    }
    case A_codeBlockStmtType::A_breakStmtKind:
    {
        assert(bre_label);
        emit_irs.push_back(L_Jump(bre_label));
        break;
    }
    case A_codeBlockStmtType::A_callStmtKind:
    {
        string funcName = *b->u.callStmt->fnCall->fn;
        vector<AS_operand *> args;
        for (const auto &val : b->u.callStmt->fnCall->vals)
        {
            args.push_back(ast2llvmRightVal(val));
        }
        emit_irs.push_back(L_Voidcall(funcName, args));
        break;
    }
    case A_codeBlockStmtType::A_continueStmtKind:
    {
        assert(con_label);
        emit_irs.push_back(L_Jump(con_label));
        break;
    }
    case A_codeBlockStmtType::A_ifStmtKind:
    {
        Temp_label *if_label = Temp_newlabel();
        Temp_label *else_label = Temp_newlabel();
        Temp_label *after_label = Temp_newlabel();
        ast2llvmBoolExpr(b->u.ifStmt->boolExpr, if_label, else_label);
        emit_irs.push_back(L_Label(if_label));
        for (aA_codeBlockStmt stmt : b->u.ifStmt->ifStmts)
        {
            ast2llvmBlock(stmt, con_label, bre_label);
        }
        emit_irs.push_back(L_Jump(after_label));
        emit_irs.push_back(L_Label(else_label));
        for (aA_codeBlockStmt stmt : b->u.ifStmt->elseStmts)
        {
            ast2llvmBlock(stmt, con_label, bre_label);
        }
        emit_irs.push_back(L_Jump(after_label));
        emit_irs.push_back(L_Label(after_label));
        break;
    }
    case A_codeBlockStmtType::A_returnStmtKind:
    {
        emit_irs.push_back(L_Ret(ast2llvmRightVal(b->u.returnStmt->retVal)));
        break;
    }
    case A_codeBlockStmtType::A_varDeclStmtKind:
    {
        if (b->u.varDeclStmt->kind == A_varDeclStmtType::A_varDeclKind)
        {
            if (b->u.varDeclStmt->u.varDecl->kind == A_varDeclType::A_varDeclScalarKind)
            {
                string id = *b->u.varDeclStmt->u.varDecl->u.declScalar->id;
                Temp_temp *var;
                if (b->u.varDeclStmt->u.varDecl->u.declScalar->type->type == A_dataType::A_nativeTypeKind)
                {
                    var = Temp_newtemp_int_ptr(0);
                    localVarMap.emplace(id, var);
                    emit_irs.push_back(L_Alloca(AS_Operand_Temp(var)));
                }
                else if (b->u.varDeclStmt->u.varDecl->u.declScalar->type->type == A_dataType::A_structTypeKind)
                {
                    var = Temp_newtemp_struct_ptr(0, *b->u.varDeclStmt->u.varDecl->u.declScalar->type->u.structType);
                    localVarMap.emplace(id, var);
                    emit_irs.push_back(L_Alloca(AS_Operand_Temp(var)));
                }
            }
            else if (b->u.varDeclStmt->u.varDecl->kind == A_varDeclType::A_varDeclArrayKind)
            {
                int len = b->u.varDeclStmt->u.varDecl->u.declArray->len;
                string id = *b->u.varDeclStmt->u.varDecl->u.declArray->id;
                Temp_temp *var;
                if (b->u.varDeclStmt->u.varDecl->u.declArray->type->type == A_dataType::A_nativeTypeKind)
                {
                    var = Temp_newtemp_int_ptr(len);
                    localVarMap.emplace(id, var);
                    emit_irs.push_back(L_Alloca(AS_Operand_Temp(var)));
                }
                else if (b->u.varDeclStmt->u.varDecl->u.declArray->type->type == A_dataType::A_structTypeKind)
                {
                    var = Temp_newtemp_struct_ptr(len, *b->u.varDeclStmt->u.varDecl->u.declArray->type->u.structType);
                    localVarMap.emplace(id, var);
                    emit_irs.push_back(L_Alloca(AS_Operand_Temp(var)));
                }
            }
        }
        else if (b->u.varDeclStmt->kind == A_varDeclStmtType::A_varDefKind)
        {
            if (b->u.varDeclStmt->u.varDef->kind == A_varDefType::A_varDefScalarKind)
            {
                string id = *b->u.varDeclStmt->u.varDef->u.defScalar->id;
                aA_rightVal val = b->u.varDeclStmt->u.varDef->u.defScalar->val;
                Temp_temp *var;
                AS_operand *dst;
                if (b->u.varDeclStmt->u.varDef->u.defScalar->type->type == A_dataType::A_nativeTypeKind)
                {
                    var = Temp_newtemp_int_ptr(0);
                    localVarMap.emplace(id, var);
                    dst = AS_Operand_Temp(var);
                    emit_irs.push_back(L_Alloca(dst));
                    emit_irs.push_back(L_Store(ast2llvmRightVal(val), dst));
                }
                else if (b->u.varDeclStmt->u.varDef->u.defScalar->type->type == A_dataType::A_structTypeKind)
                {
                    var = Temp_newtemp_struct_ptr(0, *b->u.varDeclStmt->u.varDef->u.defScalar->type->u.structType);
                    localVarMap.emplace(id, var);
                    dst = AS_Operand_Temp(var);
                    emit_irs.push_back(L_Alloca(dst));
                }
            }
            else if (b->u.varDeclStmt->u.varDef->kind == A_varDefType::A_varDefArrayKind)
            {
                string id = *b->u.varDeclStmt->u.varDef->u.defArray->id;
                int len = b->u.varDeclStmt->u.varDef->u.defArray->len;
                vector<aA_rightVal> vals = b->u.varDeclStmt->u.varDef->u.defArray->vals;
                Temp_temp *var;
                AS_operand *base;
                if (b->u.varDeclStmt->u.varDef->u.defArray->type->type == A_dataType::A_nativeTypeKind)
                {
                    var = Temp_newtemp_int_ptr(len);
                    localVarMap.emplace(id, var);
                    base = AS_Operand_Temp(var);
                    emit_irs.push_back(L_Alloca(base));
                    for (int i = 0; i < len; i++)
                    {
                        var = Temp_newtemp_int_ptr(0);
                        AS_operand *target = AS_Operand_Temp(var);
                        //对数组中的元素赋值
                        emit_irs.push_back(L_Gep(target, base, AS_Operand_Const(i)));
                        emit_irs.push_back(L_Store(ast2llvmRightVal(vals[i]), target));
                    }
                }
                else if (b->u.varDeclStmt->u.varDef->u.defArray->type->type == A_dataType::A_structTypeKind)
                {
                    var = Temp_newtemp_struct_ptr(len, *b->u.varDeclStmt->u.varDef->u.defArray->type->u.structType);
                    localVarMap.emplace(id, var);
                    base = AS_Operand_Temp(var);
                    emit_irs.push_back(L_Alloca(base));
                    for (int i = 0; i < len; i++)
                    {
                        var = Temp_newtemp_struct_ptr(0, *b->u.varDeclStmt->u.varDef->u.defArray->type->u.structType);
                        AS_operand *target = AS_Operand_Temp(var);
                          //对数组中的元素赋值
                        emit_irs.push_back(L_Gep(target, base, AS_Operand_Const(i)));
                    }
                }
            }
        }
        break;
    }
    case A_codeBlockStmtType::A_whileStmtKind:
    {
        Temp_label *while_label = Temp_newlabel();
        emit_irs.push_back(L_Jump(while_label));
        emit_irs.push_back(L_Label(while_label));
        Temp_label *codeblock_label = Temp_newlabel();
        Temp_label *break_label = Temp_newlabel();
        ast2llvmBoolExpr(b->u.whileStmt->boolExpr, codeblock_label, break_label);
        emit_irs.push_back(L_Label(codeblock_label));
        for (auto stmt : b->u.whileStmt->whileStmts)
            ast2llvmBlock(stmt, while_label, break_label);
        emit_irs.push_back(L_Jump(while_label));
        emit_irs.push_back(L_Label(break_label));
    }
    break;
    default:
        break;
    }
}

AS_operand *ast2llvmRightVal(aA_rightVal r)
{
    if (r == nullptr)
        return nullptr;
    if (r->kind == A_rightValType::A_arithExprValKind)
    {
        return ast2llvmArithExpr(r->u.arithExpr);
    }
    else if (r->kind == A_rightValType::A_boolExprValKind)
    {
        Temp_label *true_label = Temp_newlabel();
        Temp_label *false_label = Temp_newlabel();
        Temp_label *after_label = Temp_newlabel();
        AS_operand *bool_var = AS_Operand_Temp(Temp_newtemp_int_ptr(0));
        emit_irs.push_back(L_Alloca(bool_var));
        ast2llvmBoolExpr(r->u.boolExpr, true_label, false_label);
        emit_irs.push_back(L_Label(true_label));
        emit_irs.push_back(L_Store(AS_Operand_Const(1), bool_var));
        emit_irs.push_back(L_Jump(after_label));
        emit_irs.push_back(L_Label(false_label));
        emit_irs.push_back(L_Store(AS_Operand_Const(0), bool_var));
        emit_irs.push_back(L_Jump(after_label));
        emit_irs.push_back(L_Label(after_label));
        AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
        emit_irs.push_back(L_Load(res, bool_var));
        return res;
    }
    else
        assert(0);
}

AS_operand *ast2llvmLeftVal(aA_leftVal l)
{
    if (l->kind == A_leftValType::A_varValKind)
    {
        string id = *l->u.id;
        if (localVarMap.find(id) != localVarMap.end())
        {
            return AS_Operand_Temp(localVarMap[id]);
        }
        else if (globalVarMap.find(id) != globalVarMap.end())
        {
            return AS_Operand_Name(globalVarMap[id]);
        }
    }
    else if (l->kind == A_leftValType::A_arrValKind)
    {
        AS_operand *index = ast2llvmIndexExpr(l->u.arrExpr->idx);
        AS_operand *arr = ast2llvmLeftVal(l->u.arrExpr->arr);
        AS_operand *res;
        if (arr->kind == OperandKind::NAME)
        {
            if (arr->u.NAME->type == TempType::INT_PTR)
            {
                res = AS_Operand_Temp(Temp_newtemp_int_ptr(0));
            }
            else if (arr->u.NAME->type == TempType::STRUCT_PTR)
            {
                res = AS_Operand_Temp(Temp_newtemp_struct_ptr(0, arr->u.NAME->structname));
            }
        }
        else if (arr->kind == OperandKind::TEMP)
        {
            if (arr->u.TEMP->type == TempType::INT_PTR)
            {
                res = AS_Operand_Temp(Temp_newtemp_int_ptr(0));
            }
            else if (arr->u.TEMP->type == TempType::STRUCT_PTR)
            {
                res = AS_Operand_Temp(Temp_newtemp_struct_ptr(0, arr->u.TEMP->structname));
            }
        }
        // 给数组中的值赋值
        emit_irs.push_back(L_Gep(res, arr, index));
        return res;
    }
    else if (l->kind == A_leftValType::A_memberValKind)
    {
        AS_operand *base = ast2llvmLeftVal(l->u.memberExpr->structId);
        AS_operand *res;
        AS_operand *index;
        if (base->kind == OperandKind::NAME)
        {
            // 获得struct中的offset
            int offset = structInfoMap[base->u.NAME->structname].memberinfos[*l->u.memberExpr->memberId].offset;
            TempDef def = structInfoMap[base->u.NAME->structname].memberinfos[*l->u.memberExpr->memberId].def;
            index = AS_Operand_Const(offset);
            if (base->u.NAME->type == TempType::STRUCT_TEMP)
            {
                if (def.kind == TempType::INT_TEMP)
                {
                    res = AS_Operand_Temp(Temp_newtemp_int_ptr(0));
                }
                else if (def.kind == TempType::INT_PTR)
                {
                    res = AS_Operand_Temp(Temp_newtemp_int_ptr(def.len));
                }
                else if (def.kind == TempType::STRUCT_TEMP)
                {
                    res = AS_Operand_Temp(Temp_newtemp_struct_ptr(0, def.structname));
                }
                else if (def.kind == TempType::STRUCT_PTR)
                {
                    res = AS_Operand_Temp(Temp_newtemp_struct_ptr(def.len, def.structname));
                }
            }
        }
        else if (base->kind == OperandKind::TEMP)
        {
            // 获得struct中的offset
            int offset = structInfoMap[base->u.TEMP->structname].memberinfos[*l->u.memberExpr->memberId].offset;
            TempDef def = structInfoMap[base->u.TEMP->structname].memberinfos[*l->u.memberExpr->memberId].def;
            index = AS_Operand_Const(offset);
            if (base->u.TEMP->type == TempType::STRUCT_PTR && base->u.TEMP->len == 0)
            {
                if (def.kind == TempType::INT_TEMP)
                {
                    res = AS_Operand_Temp(Temp_newtemp_int_ptr(0));
                }
                else if (def.kind == TempType::INT_PTR)
                {
                    res = AS_Operand_Temp(Temp_newtemp_int_ptr(def.len));
                }
                else if (def.kind == TempType::STRUCT_TEMP)
                {
                    res = AS_Operand_Temp(Temp_newtemp_struct_ptr(0, def.structname));
                }
                else if (def.kind == TempType::STRUCT_PTR)
                {
                    res = AS_Operand_Temp(Temp_newtemp_struct_ptr(def.len, def.structname));
                }
            }
        }
        // 给struct中的值赋值
        emit_irs.push_back(L_Gep(res, base, index));
        return res;
    }
    assert(0);
}

AS_operand *ast2llvmIndexExpr(aA_indexExpr index)
{
    if (index->kind == A_indexExprKind::A_numIndexKind)
    {
        return AS_Operand_Const(index->u.num);
    }
    else if (index->kind == A_indexExprKind::A_idIndexKind)
    {
        string id = *index->u.id;
        if (localVarMap.find(id) != localVarMap.end())
        {
            AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
            AS_operand *mid = AS_Operand_Temp(localVarMap[id]);
            // 如果 id是int类型，直接返回
            if (localVarMap[id]->type == TempType::INT_TEMP)
            {
                return mid;
            }
            // 如果 id是int_ptr类型，需要先L_Load
            emit_irs.push_back(L_Load(res, mid));
            return res;
        }
        else if (globalVarMap.find(id) != globalVarMap.end())
        {
            AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
            emit_irs.push_back(L_Load(res, AS_Operand_Name(globalVarMap[id])));
            return res;
        }
    }
    else
    {
        assert(0);
    }
}

AS_operand *ast2llvmBoolExpr(aA_boolExpr b, Temp_label *true_label, Temp_label *false_label)
{
    assert(true_label && false_label);
    switch (b->kind)
    {
    case A_boolExprType::A_boolBiOpExprKind:
        ast2llvmBoolBiOpExpr(b->u.boolBiOpExpr, true_label, false_label);
        break;
    case A_boolExprType::A_boolUnitKind:
        ast2llvmBoolUnit(b->u.boolUnit, true_label, false_label);
        break;
    default:
        assert(0);
    }
    return nullptr;
}

void ast2llvmBoolBiOpExpr(aA_boolBiOpExpr b, Temp_label *true_label, Temp_label *false_label)
{
    Temp_label *right_label = Temp_newlabel();
    switch (b->op)
    {
    case A_boolBiOp::A_and:
        ast2llvmBoolExpr(b->left, right_label, false_label);
        break;
    case A_boolBiOp::A_or:
        ast2llvmBoolExpr(b->left, true_label, right_label);
        break;
    default:
        assert(0);
    }
    emit_irs.push_back(L_Label(right_label));
    ast2llvmBoolExpr(b->right, true_label, false_label);
}

void ast2llvmBoolUnit(aA_boolUnit b, Temp_label *true_label, Temp_label *false_label)
{
    switch (b->kind)
    {
    case A_boolUnitType::A_boolExprKind:
        ast2llvmBoolExpr(b->u.boolExpr, true_label, false_label);
        break;
    case A_boolUnitType::A_boolUOpExprKind:
        ast2llvmBoolUOpExpr(b->u.boolUOpExpr, true_label, false_label);
        break;
    case A_boolUnitType::A_comOpExprKind:
        ast2llvmComOpExpr(b->u.comExpr, true_label, false_label);
        break;
    default:
        assert(0);
    }
}

void ast2llvmBoolUOpExpr(aA_boolUOpExpr b, Temp_label *true_label, Temp_label *false_label)
{
    ast2llvmBoolUnit(b->cond, false_label, true_label);
}

void ast2llvmComOpExpr(aA_comExpr c, Temp_label *true_label, Temp_label *false_label)
{
    AS_operand *leftexpr = ast2llvmExprUnit(c->left);
    AS_operand *rightexpr = ast2llvmExprUnit(c->right);
    L_relopKind op;
    AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
    switch (c->op)
    {
    case A_comOp::A_lt:
        op = L_relopKind::T_lt;
        break;
    case A_comOp::A_le:
        op = L_relopKind::T_le;
    case A_comOp::A_gt:
        op = L_relopKind::T_gt;
        break;
    case A_comOp::A_ge:
        op = L_relopKind::T_ge;
        break;
    case A_comOp::A_eq:
        op = L_relopKind::T_eq;
        break;
    case A_comOp::A_ne:
        op = L_relopKind::T_ne;
        break;
    default:
        assert(0);
    }
    emit_irs.push_back(L_Cmp(op, leftexpr, rightexpr, res));
    emit_irs.push_back(L_Cjump(res, true_label, false_label));
}

AS_operand *ast2llvmArithBiOpExpr(aA_arithBiOpExpr a)
{
    AS_operand *l = ast2llvmArithExpr(a->left);
    AS_operand *r = ast2llvmArithExpr(a->right);
    AS_operand *dst = AS_Operand_Temp(Temp_newtemp_int());
    switch (a->op)
    {
    case A_add:
    {
        emit_irs.push_back(L_Binop(L_binopKind::T_plus, l, r, dst));
        return dst;
        break;
    }
    case A_sub:
    {
        emit_irs.push_back(L_Binop(L_binopKind::T_minus, l, r, dst));
        return dst;
        break;
    }
    case A_mul:
    {
        emit_irs.push_back(L_Binop(L_binopKind::T_mul, l, r, dst));
        return dst;
        break;
    }
    case A_div:
    {
        emit_irs.push_back(L_Binop(L_binopKind::T_div, l, r, dst));
        return dst;
        break;
    }
    default:
        assert(0);
        break;
    }
}

AS_operand *ast2llvmArithUExpr(aA_arithUExpr a)
{
    AS_operand *expr = ast2llvmExprUnit(a->expr);
    if (a->op == A_neg)
    {
        Temp_temp *temp = Temp_newtemp_int();
        AS_operand *dst = AS_Operand_Temp(temp);
        emit_irs.push_back(L_Binop(L_binopKind::T_minus, AS_Operand_Const(0), expr, dst));
        return dst;
    }
    assert(0);
}

AS_operand *ast2llvmArithExpr(aA_arithExpr a)
{
    switch (a->kind)
    {
    case A_arithExprType::A_arithBiOpExprKind:
        return ast2llvmArithBiOpExpr(a->u.arithBiOpExpr);
        break;
    case A_arithExprType::A_exprUnitKind:
        return ast2llvmExprUnit(a->u.exprUnit);
        break;
    default:
        assert(0);
    }
    return nullptr;
}

AS_operand *ast2llvmExprUnit(aA_exprUnit e)
{
    switch (e->kind)
    {
    case A_exprUnitType::A_arithExprKind:
        return ast2llvmArithExpr(e->u.arithExpr);
        break;
    case A_exprUnitType::A_arithUExprKind:
        return ast2llvmArithUExpr(e->u.arithUExpr);
        break;
    case A_exprUnitType::A_fnCallKind:
    {
        AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
        vector<AS_operand *> args;
        for (auto val : e->u.callExpr->vals)
        {
            args.push_back(ast2llvmRightVal(val));
        }
        emit_irs.push_back(L_Call(*e->u.callExpr->fn, res, args));
        return res;
    }
    break;
    case A_exprUnitType::A_numExprKind:
        return AS_Operand_Const(e->u.num);
        break;
    case A_exprUnitType::A_idExprKind:
    {
        if (localVarMap.find(*e->u.id) != localVarMap.end())
        {
            AS_operand *temp = AS_Operand_Temp(localVarMap[*e->u.id]);
            if (localVarMap[*e->u.id]->type == TempType::INT_PTR && localVarMap[*e->u.id]->len == 0)
            {
                AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
                emit_irs.push_back(L_Load(res, temp));
                return res;
            }
            else
                return temp;
        }
        else if (globalVarMap.find(*e->u.id) != globalVarMap.end())
        {
            AS_operand *temp = AS_Operand_Name(globalVarMap[*e->u.id]);
            if (globalVarMap[*e->u.id]->type == TempType::INT_TEMP)
            {
                AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
                emit_irs.push_back(L_Load(res, temp));
                return res;
            }
            else
                return temp;
        }
        else
            assert(0);
    }
    break;
    case A_exprUnitType::A_arrayExprKind:
    {
        AS_operand *index = ast2llvmIndexExpr(e->u.arrayExpr->idx);
        AS_operand *base = ast2llvmLeftVal(e->u.arrayExpr->arr);
        AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
        AS_operand *mid = AS_Operand_Temp(Temp_newtemp_int_ptr(0));
        emit_irs.push_back(L_Gep(mid, base, index));
        emit_irs.push_back(L_Load(res, mid));
        return res;
    }
    break;
    case A_exprUnitType::A_memberExprKind:
    {
        AS_operand *struc = ast2llvmLeftVal(e->u.memberExpr->structId);
        AS_operand *mid = AS_Operand_Temp(Temp_newtemp_int_ptr(0));
        AS_operand *res = AS_Operand_Temp(Temp_newtemp_int());
        if (struc->kind == OperandKind::NAME)
        {
            string structName = struc->u.NAME->structname;
            int offset = structInfoMap[structName].memberinfos[*e->u.memberExpr->memberId].offset;
            emit_irs.push_back(L_Gep(mid, struc, AS_Operand_Const(offset)));
            emit_irs.push_back(L_Load(res, mid));
            return res;
        }
        else if (struc->kind == OperandKind::TEMP)
        {
            string structName = struc->u.TEMP->structname;
            int offset = structInfoMap[structName].memberinfos[*e->u.memberExpr->memberId].offset;
            emit_irs.push_back(L_Gep(mid, struc, AS_Operand_Const(offset)));
            emit_irs.push_back(L_Load(res, mid));
            return res;
        }
        else
            assert(0);
    }
    break;

    default:
        assert(0);
    }
}

LLVMIR::L_func *ast2llvmFuncBlock(Func_local *f)
{
    list<L_block *> blocks;
    assert(f->irs.front()->type == L_StmKind::T_LABEL);
    list<L_stm *> ins;
    for (auto ir : f->irs)
    {
        if (ir->type == L_StmKind::T_LABEL && !ins.empty())
        {
            blocks.emplace_back(L_Block(ins));
            ins.clear();
        }
        ins.emplace_back(ir);
    }
    if (!ins.empty())
    {
        blocks.emplace_back(L_Block(ins));
        ins.clear();
    }
    return new L_func(f->name, f->ret, f->args, blocks);
}

void ast2llvm_moveAlloca(LLVMIR::L_func *f)
{
    L_block *first = f->blocks.front();
    list<L_stm *> new_first_instrs;
    for (L_block *block : f->blocks)
    {
        auto iter = block->instrs.begin();
        while (iter != block->instrs.end())
        {
            if ((*iter)->type == L_StmKind::T_ALLOCA)
            {
                new_first_instrs.push_back(*iter);
                iter = block->instrs.erase(iter);
            }
            else
                iter++;
        }
    }
    for (L_stm *ir : first->instrs)
    {
        if (ir->type == L_StmKind::T_LABEL)
            new_first_instrs.insert(new_first_instrs.begin(), ir);
        else if (ir->type != L_StmKind::T_ALLOCA)
            new_first_instrs.push_back(ir);
    }
    first->instrs = new_first_instrs;
}