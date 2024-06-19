package body Literals is
    function Stringify (L : Literal) return String is
    begin
        case L.Kind is
            when Nothing =>
                return "nil";

            when Float_Type =>
                declare
                    Number_Text : String := L.Float_Val'Image;
                    Last_Text   : String :=
                       Number_Text
                          ((Number_Text'Last - 1) .. Number_Text'Last);
                begin
                    if Last_Text = ".0" then
                        Number_Text := Number_Text (1 .. Number_Text'Last - 2);
                    end if;
                    return Number_Text;
                end;
            when Bool_Type =>
                return L.Bool_Val'Image;
            when String_Type =>
                return L.String_Val.To_String;
            when others =>
                return "";
        end case;
    end Stringify;

    function Is_Truthy (L : Literal) return Boolean is
    begin
        case L.Kind is
            when Nothing =>
                return False;
            when Bool_Type =>
                return False;
            when others =>
                return True;
        end case;
    end Is_Truthy;

    function Is_Equal (A : Literal; B : Literal) return Boolean is
    begin
        if A.Kind = Nothing and B.Kind = Nothing then
            return True;
        end if;

        if A.Kind = Nothing then
            return False;
        end if;
        return A = B;
    end Is_Equal;
end Literals;
