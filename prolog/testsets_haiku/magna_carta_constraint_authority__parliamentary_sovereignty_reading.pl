% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_parliamentary_sovereignty, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Restraints via Parliamentary Statute (Sovereignty Reading)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   Magna Carta emerges from medieval feudalism as a restraint on Crown
 *   prerogative, but by the early modern period it exists only as interpreted
 *   into parliamentary statute law. This constraint instantiates the
 *   'parliamentary sovereignty' reading of that kernel: Crown authority is
 *   constrained by law, but that law is statute passed by Parliament, which
 *   retains the power to revise, repeal, or rewrite any charter provision.
 *   This reading creates an asymmetric distribution: Parliament as the
 *   guardian of restraint (and beneficiary of institutional power) vs.
 *   permanent minorities whose protection depends entirely on parliamentary
 *   will. The constraint is claimed as tangled_rope because it genuinely
 *   coordinates restraint on Crown prerogative but does so via a structure
 *   (parliamentary majoritarianism) that itself becomes a vector for
 *   extraction. The reading competes with two siblings: living
 *   constitutionalism (inherited precedent binds all successors, even
 *   parliament) and feudal obsolescence (the charter is historically
 *   superseded and legally irrelevant). This story instantiates only the
 *   parliamentary sovereignty reading and routes the contest to omega
 *   variables.
 *
 * KEY AGENTS:
 *   - Parliament as Institution: agenda-setter and institutional beneficiary; controls which charter restraints persist via statute
 *   - Crown Executive: powerful but trapped payer; constrained by parliamentary statute but depends on parliament for any governance capacity
 *   - Majoritarian Coalitions in Parliament: organized beneficiaries; hold temporary parliamentary dominance and thereby control constraint revision
 *   - Permanent Minorities: powerless victims with identity-locked exit; depend entirely on parliament's good will for charter protection
 *   - Unrepresented Subjects: powerless victims historically excluded from parliament; bear costs of majoritarian legislation with no say in charter revision
 *   - Common Law Judiciary: institutional observer and conditional beneficiary; enforce statutory limits on Crown but remain subject to parliamentary override
 *   - Rival Constitutional Authorities: excluded advocates of entrenchment; their frameworks would require parliament to surrender some sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.49).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.49).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Restraints via Parliamentary Statute (Sovereignty Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '5b369bb2-749c-4de1-9ee5-e9483ba4c4ec').
narrative_ontology:cs_kernel_codification('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', fixed_text).
narrative_ontology:cs_authority_grounding('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', lineage).
narrative_ontology:cs_interpretation_layer_present('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec').
narrative_ontology:cs_reading_relation('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', foundational, parliamentary_sovereignty_supreme).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_supreme, holdable).
narrative_ontology:cs_axiom_grounding('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', parliamentary_sovereignty_supreme, conventional).
narrative_ontology:cs_axiom('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', foundational, charter_authority_mediated_through_statute).
narrative_ontology:cs_axiom_status(charter_authority_mediated_through_statute, holdable).
narrative_ontology:cs_axiom_grounding('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', charter_authority_mediated_through_statute, conventional).
narrative_ontology:cs_reference_frame('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', parliamentary_statute_as_charter_medium).
narrative_ontology:cs_drift_state('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b369bb2-749c-4de1-9ee5-e9483ba4c4ec', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament_as_institution).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majoritarian_coalitions_in_parliament).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, permanent_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_subjects_constituencies).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits moderate extractiveness (0.58) because the core benefit—restraint on Crown prerogative—is real and partially distributed (judges enforce it, common law tradition preserves it, even minorities nominally enjoy charter protections). But the measurement reflects the asymmetry: minorities gain protection only conditionally on parliamentary will, and parliament has historically revised charter provisions against minority interests (suspension of habeas corpus, discriminatory statutes, colonial legislation). Suppression is lower than extractiveness (0.49) because the mechanism is formal and publicly visible: parliament openly debates statute, votes are recorded, the legislative process is not hidden. However, suppression persists because minorities lack the parliamentary power to block revisionary legislation. Theater ratio (0.31) indicates that significant parliamentary activity is genuine restraint work (enforcement against Crown overreach, judges reviewing executive action) but a growing proportion is defensive maintenance of sovereignty itself—rehearsing arguments about why parliament's power is legitimate, why alternatives would be worse—as international human rights and constitutional reform movements challenge the reading. The measurement trajectory shows extractiveness rising from 0.38 to 0.60 over 40 years as parliament has faced increasing pressure from human rights conventions, devolution demands, and supermajority calls, leading to more explicit exercise of revisionary power (and theater) to defend the framework. Suppression_requirement rises similarly because parliament must actively suppress constitutional reform movements to maintain pure sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's seat: this is genuine coordination. The institution inherited the crown's old problem (arbitrary rule) and the charter's old solution (restraint through law); parliament stabilized restraint by making it statute law. Parliament sees itself as the guardian of chartered freedoms and the only authority that can coherently defend them. From the Crown's seat: constraint is real but conditional. The executive is bound by statute but depends on parliamentary consent for any major action; the constraint is asymmetric (parliament can change the rules unilaterally, Crown cannot). From permanent-minority seats: the constraint is a conditional promise. They depend entirely on parliament's current composition; if parliament votes to revise a protection, they have no recourse except civil resistance or exit (which is identity-locked). From rival-constitutional-authority seats (advocates of entrenchment): this is not a constraint at all but a hollow claim. True constraint requires that parliament itself be bound by something higher than statute (written constitution, fundamental law, judicial supremacy). The engine computes per-seat type from these structural differences: Parliament perceives tangled_rope or rope; Crown perceives snare or tangled_rope (depending on how much executive authority parliament actually retains); minorities perceive snare (majoritarian extraction with no exit). The authored metrics reflect the weighted average across seats, anchoring on the highest-extraction seats (minorities).
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament is the beneficiary (institutional power, revisionary authority, legitimacy as representative body) with d near 0.2-0.3 (low extraction, structural benefit). Majoritarian coalitions gain d ~0.1-0.2 (temporary benefit from holding majority). Crown is constrained (d ~0.7, high extraction via statute, trapped exit). Permanent minorities have d ~0.9 (effective targets: they depend on parliament for protection, have identity-locked exit, face suppression if they resist parliamentary will). Unrepresented constituencies have d ~0.85 (similar to minorities but with slightly more potential for constrained exit via emigration or demographic change). The beneficiary declaration (parliament, majoritarian coalitions) drives the coordination-function claim; the victim declaration (permanent minorities, unrepresented constituencies) drives the extractiveness measurement upward and anchors the tangled_rope claim (genuine coordination but asymmetric distribution). No directionality overrides are needed; the structural data maps clearly to the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary Crown rule) has been substantially solved. Early-interval measurements (time 0-5) show lower extractiveness and theater, suggesting genuine coordination function. But the founding-problem status is contested: is the problem still live (Crown remains a potential threat requiring parliamentary sovereignty) or dead (the executive has internalized restraint and could accept constitutional limits)? If dead, then parliament's ongoing sovereignty is maintained theatrically, not functionally—hence the rising theater_ratio toward interval end. The mandatrophy question crystallizes: does parliament maintain sovereignty because Crown prerogative remains a real threat (live problem), or does parliament maintain sovereignty because it benefits from the revisionary power even though the original threat is contained (dead problem, zombie constraint)? The disappearance_verdict (world_rearranges) supports the live-problem reading: if parliament lost revisionary power via constitutional entrenchment, governance would be substantially reorganized. But the contested founding-problem-status admits the alternative: parliament might simply be defending institutional power that is no longer functionally necessary for restraint. The omega variables on majoritarianism vs. constraint (whether this reading actually constrains or just displaces abuse) speaks directly to mandatrophy: if the reading trades Crown arbitrariness for majoritarian abuse, then it hasn't solved the founding problem at all—it has merely moved it. That uncertainty is irreducible and belongs in omega space, not in a definitive mandatrophy verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Magna Carta kernel is most structurally accurate: the parliamentary sovereignty reading (this one), the living constitutionalism reading (inherited precedent binding all successors), or the feudal obsolescence reading (charter irrelevant to modern sovereignty)?',
    'Comparative analysis of constitutional practice across jurisdictions: those with parliamentary supremacy (UK, New Zealand) vs. those with constitutional entrenchment (US, Canada, Australia post-1999) show different trajectories. The lived constraint differs by reading.',
    'If living constitutionalism is correct, the constraint type shifts from tangled_rope toward mountain (inherited law has binding power); if feudal obsolescence is correct, the constraint type collapses toward piton (theater only). This measurement exists to detect which reading the corpus actually instantiates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which sibling reading best describes the actual constraint at stake.').

omega_variable(
    parliamentary_revisionary_power_exercise,
    'How often and under what conditions does parliament actually exercise its power to revise or repeal charter-derived statutory restraints on crown prerogative? Is the power mostly theoretical or actively deployed?',
    'Historical audit of statutory repeals, emergency powers suspension, and charter-protective legislation over the interval. Track moments when parliament suspended habeas corpus, expanded crown emergency authority, or narrowed charter protections via statute.',
    'High exercise frequency would increase effective extractiveness and suppression (parliament uses its revisionary power to benefit majorities at minorities'' expense); low exercise frequency would suggest the charter restraints are effectively constitutional (not subject to easy revision) despite the reading''s claim of parliamentary sovereignty. The reading assumes revisionary power is meaningful; if rarely used, the reading misconstrues the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_revisionary_power_exercise, empirical, 'Frequency and pattern of parliamentary exercise of its sovereignty over charter restraints.').

omega_variable(
    minority_protection_outside_parliament,
    'To what extent do permanent minorities and excluded constituencies find protection in common law tradition, judicial review, or extra-parliamentary norms, independently of parliament''s good will?',
    'Historical case analysis: instances where courts protected minorities against parliamentary majorities (e.g., sex discrimination, racial discrimination, religious conscience cases). Track whether judicial protection depended on parliament''s tolerance or asserted independent constitutional authority.',
    'Strong independent judicial protection would reduce the measured extractiveness and suppression for minority seats (their exit is less completely blocked). Weak judicial protection would confirm the reading''s depiction of minorities as entirely dependent on parliamentary will. The resolution determines whether the constraint is truly tangled_rope (requiring parliament as the only restraint) or whether judicial-constitutional tradition provides a secondary restraint independent of sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_outside_parliament, empirical, 'Whether minorities have structural protections outside parliamentary supremacy.').

omega_variable(
    supremacy_framework_revision_pressure,
    'Is the parliamentary sovereignty reading under pressure from alternative constitutional frameworks (written constitutions, human rights conventions, supermajority amendment rules, constitutional courts)? Is this reading''s reign generationally stable or eroding?',
    'Track constitutional reform movements, human rights convention adoption, devolution settlements, and supermajority demands in the UK and Commonwealth. Observe whether newer jurisdictions (Australia post-1999, NZ post-Bill of Rights 1990) maintain pure parliamentary sovereignty or adopt entrenched frameworks.',
    'If the reading is under sustained pressure, theater_ratio may be rising (the reading persists as defense of parliamentary tradition rather than as living constraint on executive power). If stable, the reading represents genuine ongoing structural choice. Terminal attractor analysis: is the reading''s trajectory toward pure sovereignty or toward constitutional entrenchment?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supremacy_framework_revision_pressure, empirical, 'Stability and historical trajectory of the parliamentary sovereignty reading as a constitutional framework.').

omega_variable(
    majoritarianism_vs_constraint,
    'Does this reading''s framework (restraint on Crown via Parliament, but Parliament itself unlimited) actually constrain executive power, or does it merely displace the locus of potential abuse from Crown to parliamentary majority?',
    'Examine instances of majority-driven abuse of power through statute (e.g., discriminatory legislation, suspension of minority protections, expansion of executive war powers with parliamentary approval). Compare the frequency and scope of such abuse under this reading versus under frameworks with entrenched limits on parliamentary power.',
    'If majoritarian abuse via statute is frequent and severe, the reading trades Crown prerogative arbitrariness for parliamentary majoritarianism—a lateral displacement, not a genuine constraint. The extractiveness measurement would be more accurate as high (0.58) because minorities face systematic majoritarian extraction with no recourse. If rare, the reading achieves its stated purpose. The answer determines whether the constraint type is truly tangled_rope (coordination with asymmetric distribution) or more accurately snare (pure majoritarian extraction with no exit for minorities).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(majoritarianism_vs_constraint, empirical, 'Whether the constraint actually limits executive arbitrariness or merely redistributes it to parliament.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(magn_tr_t5, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(magn_tr_t10, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(magn_tr_t15, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(magn_tr_t25, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(magn_tr_t35, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 35, 0.33).
narrative_ontology:measurement(magn_tr_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(magn_be_t5, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(magn_be_t10, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(magn_be_t15, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(magn_be_t25, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(magn_be_t35, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 35, 0.6).
narrative_ontology:measurement(magn_be_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(magn_su_t5, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(magn_su_t10, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(magn_su_t15, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(magn_su_t25, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(magn_su_t35, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement(magn_su_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.49).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_law_judicial_authority).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_statute_supremacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Magna Carta kernel (constraint_id prefix: magna_carta_constraint_authority). The sibling readings are feudal_obsolescence_reading and living_constitutionalism_reading. All three readings address the same historical kernel but produce different constraint types due to different axiomatic claims about the binding power of law and the scope of parliamentary sovereignty. The network links this reading to its siblings and to downstream constraints that depend on the parliamentary sovereignty framework (common law judicial authority, parliamentary statute supremacy). The constraint family exists because a single natural-language concept ('Magna Carta as a constitutional constraint') decomposes into three structurally distinct claims with different ε values and different victim sets. Each reading is a separate story with its own ε, beneficiaries, victims, and type. Cross-family comparison will reveal which reading the empirical corpus best instantiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
