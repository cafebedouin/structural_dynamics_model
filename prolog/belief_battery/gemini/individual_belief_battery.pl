% ============================================================================
% MODULE: individual_belief_battery
% ============================================================================
% Formalizes the Individual Practical Battery into the DR Stack
% Maps metaphysical and pragmatic beliefs to constraint classifications
% ============================================================================

:- module(individual_belief_battery, [
    my_context/1,
    belief_classification/2,
    institutional_claim/2,
    type_1_error/1
]).

% --- 1. CORE CONTEXT DEFINITION ---
% Context: Moderate individual power, biographical horizon, mobile, national scope.
my_context(context(
    agent_power(individual_moderate),  
    time_horizon(biographical),        
    exit_options(mobile),              
    spatial_scope(national)            
)).

% --- 2. TIER 0: THEORY (Universal Mountains) ---
% Things that are true regardless of indexical parameters.

belief_classification(logical_necessity, mountain).
belief_classification(mathematical_truth, mountain).
belief_classification(physical_laws, mountain).
belief_classification(consciousness_substrate, mountain).
belief_classification(death_finality, mountain) :-
    belief_classification(consciousness_substrate, mountain).

% --- 3. TIER 1: PRACTICE - INDIVIDUAL ---
% Pragmatic choices and functional frames.

% AGENCY & CHANGE
belief_classification(meaningful_agency, rope).
belief_classification(personal_change, rope).
belief_classification(identity_stability, rope).

% MEANING & PURPOSE
belief_classification(life_meaning, rope).
belief_classification(suffering_meaning, rope).

% RELATIONSHIPS & COMMITMENT
belief_classification(relationship_meaning, rope).
belief_classification(family_obligations, varies).
belief_classification(marriage_obligations, rope).

% WORK & VALUE
belief_classification(work_value, varies).
belief_classification(wealth_status_pursuit, rope).

% EPISTEMIC NORMS
belief_classification(belief_norms, context_dependent).
belief_classification(moral_intuitions, rope).

% POWER & SYSTEMS
belief_classification(power_structures_natural, varies).
belief_classification(participation_in_nooses, rope).

% DEATH & RISK
belief_classification(mortality_acceptance, mountain) :- 
    belief_classification(death_finality, mountain).
belief_classification(risk_taking, rope).
belief_classification(future_planning, rope).

% TRUST & COOPERATION
belief_classification(trust_default, rope).
belief_classification(human_nature, rope).
belief_classification(defector_punishment, rope).

% IDENTITY & EXPRESSION
belief_classification(social_conformity, rope).
belief_classification(gender_sexual_identity, mountain_and_rope).
belief_classification(cultural_religious_identity, rope).

% CONSUMPTION & RESOURCES
belief_classification(consumption_level, rope).
belief_classification(experiences_vs_possessions, rope).
belief_classification(giving_obligation, rope).

% KNOWLEDGE & LEARNING
belief_classification(specialization, rope).
belief_classification(formal_education, rope).
belief_classification(curiosity_vs_practicality, rope).

% POLITICAL PARTICIPATION
belief_classification(electoral_participation, rope).
belief_classification(direct_action, rope).
belief_classification(local_vs_global, rope).

% EXISTENTIAL STANCE
belief_classification(optimism_pessimism, rope).
belief_classification(cosmic_meaning, rope).
belief_classification(having_children, rope).
belief_classification(time_allocation, rope).

% --- 4. INSTITUTIONAL NOOSE DETECTION ---
% Claims often made by systems to enable extraction.

institutional_claim(eternal_soul, mountain).
institutional_claim(cosmic_purpose, mountain).
institutional_claim(natural_hierarchy, mountain).
institutional_claim(work_as_dignity, mountain).
institutional_claim(family_sacred, mountain).

% Extraction Points (Common Type 1 Errors)
type_1_error(eternal_soul).      % Enables subjugation
type_1_error(cosmic_purpose).    % Enables control
type_1_error(natural_hierarchy). % Justifies oppression
type_1_error(work_as_dignity).   % Prevents questioning exploitation
type_1_error(family_sacred).     % Enables abuse
