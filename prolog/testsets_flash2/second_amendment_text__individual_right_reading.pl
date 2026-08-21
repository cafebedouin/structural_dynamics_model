% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms (Individual Right Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, which interprets the operative clause ('the right of the
 *   people to keep and bear Arms, shall not be infringed') as guaranteeing an
 *   individual's right to possess firearms for personal self-defense, largely
 *   independent of militia service. This reading has gained prominence
 *   through landmark Supreme Court decisions, shifting the amendment's focus
 *   from collective security to individual liberty. The constraint is claimed
 *   as a 'tangled_rope' because it provides a coordination function
 *   (individual self-defense) but also involves significant asymmetric
 *   extraction (costs borne by victims of gun violence and disarmed
 *   populations) and requires active enforcement to maintain its broad scope
 *   against regulatory efforts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.7).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment: Individual Right to Bear Arms (Individual Right Reading)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '111e4d7f-f841-4224-a529-1c2ac85c76c0').
narrative_ontology:cs_kernel_codification('111e4d7f-f841-4224-a529-1c2ac85c76c0', fixed_text).
narrative_ontology:cs_authority_grounding('111e4d7f-f841-4224-a529-1c2ac85c76c0', lineage).
narrative_ontology:cs_interpretation_layer_present('111e4d7f-f841-4224-a529-1c2ac85c76c0').
narrative_ontology:cs_reading_relation('111e4d7f-f841-4224-a529-1c2ac85c76c0', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('111e4d7f-f841-4224-a529-1c2ac85c76c0', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('111e4d7f-f841-4224-a529-1c2ac85c76c0', foundational, individual_self_defense_fundamental_right).
narrative_ontology:cs_axiom_status(individual_self_defense_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('111e4d7f-f841-4224-a529-1c2ac85c76c0', individual_self_defense_fundamental_right, deontological).
narrative_ontology:cs_axiom('111e4d7f-f841-4224-a529-1c2ac85c76c0', foundational, militia_clause_non_conditioning).
narrative_ontology:cs_axiom_status(militia_clause_non_conditioning, holdable).
narrative_ontology:cs_axiom_grounding('111e4d7f-f841-4224-a529-1c2ac85c76c0', militia_clause_non_conditioning, conventional).
narrative_ontology:cs_reference_frame('111e4d7f-f841-4224-a529-1c2ac85c76c0', post_heller_interpretation).
narrative_ontology:cs_drift_state('111e4d7f-f841-4224-a529-1c2ac85c76c0', contemporary_mass_shooting_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('111e4d7f-f841-4224-a529-1c2ac85c76c0', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_manufacturers_and_lobbyists).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmed_populations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, victims_of_gun_violence).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, public_safety_advocates).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, self_defense_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, individual_liberty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the right to own firearms for personal self-defense, free from most state-level restrictions. They actively resist new regulations and view the right as fundamental to their security and liberty. Exit options are constrained by the perceived necessity of self-defense and the social identity tied to gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Actively shape policy and legal interpretations to expand gun ownership rights, directly benefiting from increased sales and reduced regulatory burdens. They fund legal challenges and political campaigns to defend and extend the individual right reading.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_manufacturers_and_lobbyists, agenda_setter,
    institutional, generational, arbitrage, national).

% Includes felons, domestic abusers, and other groups legally prohibited from owning firearms. They bear the cost of being unable to exercise the right to self-defense, even if they face threats, and are subject to criminal penalties for possession. Their exit options are effectively trapped by legal status.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmed_populations, payer,
    powerless, biographical, trapped, local).

% Bear the direct and indirect costs of gun violence, including physical harm, psychological trauma, and loss of life. They advocate for stricter gun control measures, but their voices are often marginalized in policy debates. Their exit options are trapped by the prevalence of firearms and the lack of effective regulation.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, victims_of_gun_violence, payer,
    powerless, immediate, trapped, local).

% Advocate for gun control measures to reduce gun violence and enhance public safety. They bear the cost of legislative inaction and the societal impact of widespread firearm availability. Their exit options are constrained by the political power of gun rights advocates and the difficulty of amending constitutional interpretations.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, public_safety_advocates, payer,
    moderate, generational, constrained, national).

% Enforces existing firearms laws, often facing challenges due to the broad interpretation of individual rights. They are caught between upholding constitutional rights and ensuring public safety, often bearing the direct consequences of gun violence in their communities. Their ability to set policy is constrained by judicial rulings.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the individual right to self-defense, ensuring citizens can protect themselves and their families, thereby reducing reliance on state protection in immediate threats.
% TRANSFER_FUNCTION: Transfers the burden of self-protection to individuals, while transferring the cost of gun violence (healthcare, law enforcement, social disruption) to the broader public, and economic gains to firearms manufacturers.
% ABSENT_VOICES: Communities disproportionately affected by gun violence, particularly those in urban areas, are often excluded from the most influential policy discussions, where their calls for stricter regulation are framed as infringing on constitutional rights. Victims of domestic violence, who are often at heightened risk from armed partners, also have limited voice in shaping the interpretation of this right.
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished overnight, it would fundamentally alter firearms policy, leading to widespread state-level regulation, potentially confiscation of certain weapons, and a significant shift in the balance of power between citizens and the state regarding self-defense. The firearms industry would face severe economic disruption, and public safety debates would be reframed.
% FOUNDING_PROBLEM: The founding problem was to ensure the security of a free state by allowing citizens to maintain arms, both for militia service and for personal defense against tyranny or crime, reflecting a distrust of standing armies and a belief in an armed populace.
% FOUNDING_PROBLEM_CORROBORATION: Individual gun owners and firearms lobbyists attest the problem is live, citing ongoing threats to personal safety and liberty. Public safety advocates and some legal scholars attest the problem of tyranny is largely dead in the modern context, and the constraint now primarily facilitates gun violence; historical analysis of the amendment's drafting and early interpretations from outside the benefiting parties supports the contested nature of the founding intent.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high due to the societal costs of gun violence and the limitations placed on public safety measures. Suppression (0.7) is also high, reflecting the strong legal and political barriers to enacting stricter gun control, effectively suppressing alternative regulatory approaches. The theater ratio (0.2) is relatively low, as the individual right interpretation is actively defended and enforced, with real consequences, rather than being merely performative. Resistance (0.8) is very high, as public safety advocates and victims of gun violence actively contest this interpretation and push for reform. Accessibility collapse (0.4) is moderate, as while some alternatives (e.g., stricter gun laws) are suppressed, others (e.g., personal security measures, advocacy) still exist.
 *
 * PERSPECTIVAL GAP:
 *   The individual right reading is experienced as a fundamental liberty by its beneficiaries, while its victims experience it as a source of insecurity and extraction. The engine's classification will reflect this divergence, likely showing a 'rope' or 'scaffold' from the beneficiary seat and a 'snare' or 'tangled_rope' from the victim seats. This gap is central to the ongoing political and legal contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and firearms manufacturers are clear beneficiaries, experiencing low directionality. Disarmed populations, victims of gun violence, and public safety advocates are targets, experiencing high directionality due to the costs they bear and their constrained exit options. Law enforcement, while an agenda-setter in some respects, is also constrained by the interpretation, placing them in a more symmetric but still challenging position.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling this constraint as a 'rope' (pure coordination) by acknowledging the significant extraction and suppression involved. It also avoids mislabeling it as a 'snare' (pure extraction) by recognizing the genuine coordination function of individual self-defense, even if that function is contested and comes with high costs. The 'contested' status of the founding problem further highlights the potential for mandatrophy, where an original intent (militia for security) may have atrophied while the constraint persists with a new, more extractive function (unfettered individual gun ownership).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_relevance,
    'To what extent does the ''well regulated Militia'' clause still condition the ''right of the people to keep and bear Arms'' in contemporary society?',
    'Further Supreme Court rulings clarifying the relationship between the two clauses, or a constitutional amendment explicitly severing or re-emphasizing the militia connection.',
    'If the militia clause is deemed to have significant conditioning power, the individual right reading''s scope would narrow, potentially allowing for more state regulation. If it is further decoupled, the individual right reading would be strengthened, making regulation more difficult.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_relevance, conceptual, 'Ambiguity regarding the conditioning effect of the militia clause on the individual right.').

omega_variable(
    self_defense_necessity_empirical,
    'What is the empirical relationship between widespread individual firearm ownership and actual rates of successful self-defense versus accidental shootings and gun violence?',
    'Large-scale, longitudinal epidemiological and sociological studies comparing outcomes in jurisdictions with varying firearm access and self-defense laws.',
    'Strong evidence that widespread ownership increases overall violence or accidental harm would weaken the ''self-defense'' justification, potentially shifting the constraint towards a ''snare'' or ''tangled_rope'' even from the beneficiary seat. Evidence of clear, widespread self-defense efficacy would strengthen the ''rope'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_necessity_empirical, empirical, 'Empirical evidence on the efficacy and net societal impact of individual firearm ownership for self-defense.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gun control efforts primarily structural (e.g., legal precedents, political lobbying power) or internalized (e.g., a widespread cultural belief that gun ownership is an unassailable right)?',
    'Post-legislative-change trajectory: if gun control efforts persist and gain traction after a significant legal or political shift, reclassify as partially internalized suppression that is being overcome. If efforts immediately collapse, it''s more structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the cultural belief itself acts as a barrier to change. If purely structural, legal or political changes could more readily alter the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gun control efforts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__individual_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t10, second_amendment_text__individual_right_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(seco_tr_t20, second_amendment_text__individual_right_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(seco_tr_t30, second_amendment_text__individual_right_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(seco_tr_t40, second_amendment_text__individual_right_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(seco_tr_t50, second_amendment_text__individual_right_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__individual_right_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__individual_right_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__individual_right_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__individual_right_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(seco_be_t40, second_amendment_text__individual_right_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(seco_be_t50, second_amendment_text__individual_right_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__individual_right_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(seco_su_t10, second_amendment_text__individual_right_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(seco_su_t20, second_amendment_text__individual_right_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(seco_su_t30, second_amendment_text__individual_right_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(seco_su_t40, second_amendment_text__individual_right_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(seco_su_t50, second_amendment_text__individual_right_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, public_safety_funding).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, criminal_justice_system).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Second Amendment text kernel, alongside the 'collective_security_reading' and 'originalist_civic_virtue_reading'. Each reading instantiates a distinct constraint with different structural properties and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
