% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: State Medical Mandate Apparatus (Consent-Sovereignty Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   A state apparatus compels acceptance of medical interventions by
 *   attaching refusal consequences to employment, education, service access,
 *   and professional licensure, with exemption categories administered
 *   narrowly. This file is ONE READING of the legitimate_health_intervention
 *   kernel — the bodily_autonomy_primary reading — and it assesses the
 *   standing mandate-and-leverage arrangement through consent-primacy lenses:
 *   whatever coverage the arrangement produces, its method is the
 *   uncompensated taking of bodily decision-rights. The epsilon referent is
 *   therefore the standing coercive arrangement itself as this reading sees
 *   it, not the consent-respecting alternative the reading endorses. Sibling
 *   readings (public_health_primary, proportionality_reading) are separate
 *   files with their own epsilon, victim sets, and classifications; the
 *   contest between readings lives in the omega variables, not in this
 *   constraint's fields. KEY AGENTS (by structural relationship): -
 *   state_public_health_authorities: agenda_setter (institutional/arbitrage)
 *   — sets mandates and exemption rules, collects compliance -
 *   large_institutional_employers: beneficiary with cost-bearing secondary
 *   position (powerful/constrained) — executes termination-for-refusal under
 *   delegation - mandate_coerced_workers: primary target
 *   (moderate/constrained) — intervention-or-livelihood choice -
 *   healthcare_licensees: target (organized/trapped) — licensure-bound
 *   compliance, non-portable skills - conscientious_refusers: target
 *   (powerless/identity_locked) — identity-fused refusal, excluded from
 *   exemption design - immunocompromised_patients: incidental beneficiary
 *   (powerless/trapped) — protected by others' compliance -
 *   civil_rights_litigators: analytical observer — contests enforcement
 *   boundaries
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.75).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.7).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "State Medical Mandate Apparatus (Consent-Sovereignty Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, 'be02e4d6-aa9a-47db-a0cc-493773851177').
narrative_ontology:cs_kernel_codification('be02e4d6-aa9a-47db-a0cc-493773851177', distributed).
narrative_ontology:cs_authority_grounding('be02e4d6-aa9a-47db-a0cc-493773851177', distributed).
narrative_ontology:cs_reading_relation('be02e4d6-aa9a-47db-a0cc-493773851177', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('be02e4d6-aa9a-47db-a0cc-493773851177', legitimate_health_intervention__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('be02e4d6-aa9a-47db-a0cc-493773851177', foundational, informed_consent_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(informed_consent_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('be02e4d6-aa9a-47db-a0cc-493773851177', informed_consent_necessary_for_legitimacy, deontological).
narrative_ontology:cs_axiom('be02e4d6-aa9a-47db-a0cc-493773851177', secondary, compulsion_impermissible_regardless_of_population_benefit).
narrative_ontology:cs_axiom_status(compulsion_impermissible_regardless_of_population_benefit, holdable).
narrative_ontology:cs_axiom_grounding('be02e4d6-aa9a-47db-a0cc-493773851177', compulsion_impermissible_regardless_of_population_benefit, deontological).
narrative_ontology:cs_reference_frame('be02e4d6-aa9a-47db-a0cc-493773851177', informed_consent_sovereignty).
narrative_ontology:cs_drift_state('be02e4d6-aa9a-47db-a0cc-493773851177', contemporary_post_pandemic_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('be02e4d6-aa9a-47db-a0cc-493773851177', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, large_institutional_employers).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, immunocompromised_patients).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, healthcare_licensees).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, conscientious_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, large_institutional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues compulsory-intervention orders for workers, students, and service access; designs the exemption categories and the verification machinery; enforces through exclusion orders and delegated employer termination authority. Collects coverage rates, administrative reach, and enforcement precedent. Can amend, suspend, or rescind any element of the arrangement at will, and periodically does so under political pressure.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Implements intervention-or-termination policies across their workforces once the state delegates enforcement. Gains workforce continuity during outbreaks, reduced liability exposure, and simplified accommodation management. Bears compliance administration, attrition of skilled staff who refuse, and litigation risk. Cannot easily decline to participate where sector-wide mandates apply, and relocating operations does not escape jurisdiction-wide rules.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, large_institutional_employers, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, large_institutional_employers, payer).

% Face a choice between accepting a medical intervention they did not freely choose and losing employment, education enrollment, or access to services and venues. Switching jobs rarely escapes the rule where mandates span whole sectors; declining means forfeiting income and participation. They bear the intervention's personal risk profile and the decision-right transfer itself.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_workers, payer,
    moderate, biographical, constrained, national).

% Licensed clinicians and facility staff whose credentials tie them to regulated employers. Refusal triggers license jeopardy and termination simultaneously, and their training is non-portable outside the licensed sector. Professional associations negotiate exemption carve-outs but cannot withdraw the underlying requirement. Years of specialized training make exit into unlicensed work a severe personal loss.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, healthcare_licensees, payer,
    organized, biographical, trapped, national).

% Decline the intervention on religious or philosophical grounds that are constitutive of their identity; complying would violate commitments they treat as defining. They lose employment, schooling, and venue access, and carry social sanction labeled as anti-science or anti-community. They had no seat in designing the exemption categories that determine whether their convictions count, and exemption application processes reject most of them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, conscientious_refusers, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, conscientious_refusers, excluded).

% Medically unable to mount protective responses themselves and dependent on surrounding coverage for safety. They welcome the intervention for their own bodies where eligible and benefit further from others' compliance. Their protection depends on arrangements they cannot influence and on continued population adherence they cannot purchase individually.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, immunocompromised_patients, beneficiary,
    powerless, immediate, trapped, national).

% Challenge mandate scope, exemption adequacy, and employer delegation in court; publish analyses distinguishing medical choice from other regulated conduct. Shape the enforceable boundaries of the arrangement without collecting its coverage benefits or bearing its bodily costs directly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_rights_litigators, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level coverage of preventive interventions to interrupt transmission chains, addressing a free-rider problem in which individually rational refusal imposes infection risk on others and voluntary uptake plateaus below community-protection thresholds during outbreaks.
% TRANSFER_FUNCTION: Transfers bodily decision-authority from individuals to state and institutional authorities, enforced through employment retention, educational enrollment, venue access, and professional licensure; transfers coverage assurance and reduced outbreak exposure toward institutions and medically vulnerable populations.
% ABSENT_VOICES: Conscientious refusers and exemption-denied applicants were absent from the committees that wrote the exemption categories; unorganized gig and contract workers subject to venue-access rules lack any representative in mandate design. Courts encounter these voices only after enforcement begins, post hoc.
% DISAPPEARANCE_RATIONALE: Overnight repeal would drop coverage below thresholds in exposed sectors within weeks, forcing hospitals and large employers to improvise their own policies piecemeal; licensure-linked staff decisions would loosen; refuser seats would recover employment and access; the litigation pipeline would empty; and future outbreak response would start from an unbuilt enforcement baseline.
% FOUNDING_PROBLEM: Recurrent epidemics repeatedly exceeded hospital capacity while voluntary uptake stalled below the coverage levels needed to interrupt transmission; states built compulsory-intervention machinery, beginning with century-old school-entry requirements and extending recently into employment and access leverage, to force coverage past the voluntary plateau.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological surveillance corroborates that pathogen recurrence keeps the founding problem live; judicial dockets across multiple jurisdictions corroborate that the coercion complaint is persistent and unresolved; historical mortality records corroborate the original capacity-overwhelm problem. No corroboration comes from the authorities that administer the mandates themselves.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the arrangement takes the one asset individuals hold absolutely — bodily decision-rights — and prices refusal in livelihood and participation; the reading holds this extraction categorically, so enforcement intensity modulates reach rather than the wrongness. Suppression (0.70) is mostly structural: termination leverage, exclusion orders, narrowing exemption categories, licensure jeopardy — with a smaller internalized component (stigma and community labeling) handled by an omega. Theater ratio (0.35) is moderate-low but rising across the interval: early enforcement mapped to acute threat, while later-period activity increasingly defends the arrangement itself and processes symbolic exemption paperwork after the acute phase passed. Accessibility_collapse (0.58) reflects that alternatives (testing opt-outs, exemptions) existed but were progressively narrowed rather than eliminated. Resistance (0.62) is real: protests, mass refusal waves, litigation, and legislative reversals. Suppression_requirement is authored as a series because enforcement capacity is the dynamic being traced: an enforcement ratchet (build-up to peak) followed by partial attrition after judicial and political pushback — a single arc, not a full cycle, so seven shared-grid points suffice. All three tracked metrics are authored at all seven time points on one grid; suppression remains a raw structural property in the scalar and is never scaled by power or scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the authority seat the arrangement is coordination it administers and coverage it can measure; from the trapped licensee seat it is a forced choice binding livelihood to the body; from the identity_locked refuser seat it is compelled betrayal priced in access; from the immunocompromised seat it is protection purchased at others' expense of choice. Same structure, four incompatible lived types. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive derivation. Authorities sit nearest the beneficiary pole (they set terms, collect compliance, retain arbitrage-grade exit). Immunocompromised patients derive low directionality — genuine subsidy recipients. Coerced workers, licensees, and refusers derive high directionality, amplified by trapped and identity_locked exit: licensees' licensure trap and refusers' identity lock place them nearer the full-target end than mobile workers. One override is declared: large_institutional_employers are declared beneficiaries, but the plain derivation would underweight the costs they absorb (compliance administration, refusal-driven attrition, litigation exposure), so their d is overridden upward to 0.35 — net beneficiary, but materially cost-bearing, unlike the authority seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope guards against two symmetric misreadings. Reading the arrangement as pure rope (coordination only) erases the coerced seats — the entire delta this reading exists to register. Reading it as pure snare (extraction with cover story) denies that coverage coordination genuinely occurs and that some seats are net protected; snares suppress exits because their coordination story is false, whereas here the coordination story is true and the fight is over whether truth of the story licenses the taking. The hybrid classification keeps both halves on the table and forces the analysis through enforcement: remove the enforcement and the extraction collapses while the voluntary coordination remainder persists. On obsolescence: the founding problem is live (pathogens recur), so mandatrophy is not resolved — but the rising theater ratio flags accumulating Goodhart drift worth monitoring; if enforcement outlives the threat cycle that justifies it, the arrangement migrates toward piton dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How would this constraint''s classification change under the sibling readings of the legitimate_health_intervention kernel?',
    'Instantiate public_health_primary and proportionality_reading as separate constraint stories and compare victim sets, directionality derivations, and computed types across the three files sharing the network edge set.',
    'Under public_health_primary the mandate-coerced seats flip toward externality-imposer framing and epsilon falls toward coordination-cost levels; under proportionality_reading epsilon becomes threat-calibrated and oscillates with declared emergencies. Only this reading yields the current victim set and the high flat epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one reading of the legitimate_health_intervention kernel; sibling readings restructure the seats and reclassify the arrangement.').

omega_variable(
    consent_absolutism_emergency_boundary,
    'Does this reading concede any population-threat threshold at which compelled intervention becomes legitimate, or is consent-necessity exceptionless?',
    'Doctrinal analysis of the reading''s own tradition — research ethics already concedes narrow emergency exceptions — combined with observing whether adherents concede catastrophic-threat carve-outs in practice.',
    'If exceptionless, epsilon stays pinned high across threat cycles and the forecloses relations to both siblings hold rigidly; if emergency exceptions are conceded, epsilon acquires threat-phase variance and the reading converges structurally toward proportionality_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_absolutism_emergency_boundary, conceptual, 'Boundary of the categorical consent claim under extreme population threat.').

omega_variable(
    suppression_structural_internalized_split,
    'Of the measured suppression borne by refuser seats, how much is structural (employment and access penalties, narrowed exemptions) and how much is internalized (stigma, self-labeling, community isolation)?',
    'Post-repeal suppression trajectory: track refuser-seat costs after penalty machinery is dismantled; costs that persist after barrier removal indicate the internalized share.',
    'If the internalized share is large, effective suppression outlasts the enforcement apparatus and residual inertia persists after formal repeal — pushing the post-repeal remnant toward piton-like dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_split, empirical, 'Structural versus internalized composition of suppression on identity-locked seats.').

omega_variable(
    employment_leverage_share_of_extraction,
    'What share of the measured extraction flows through delegated private-sector leverage (employer termination, venue access rules) versus direct state compulsion?',
    'Decompose enforcement actions by lever type across the interval; track court rulings that restrict or uphold employer-delegated mandate enforcement.',
    'If leverage carries most extraction and courts constrain it, effective extraction falls sharply for private-sector seats while state-direct compulsion persists — widening seat divergence and splitting one apparent constraint into two with different epsilon profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_leverage_share_of_extraction, empirical, 'Leverage-mediated versus state-direct composition of the extraction channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 4, 0.16).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 8, 0.21).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.26).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 16, 0.3).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 20, 0.33).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, resource_allocation).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial debate 'is the state medical mandate legitimate?' decomposes into three structurally distinct constraints — one per reading of the legitimate_health_intervention kernel — because the legitimacy criterion determines the epsilon referent, the victim set, and thus the classification. This member (bodily_autonomy_primary) authors high epsilon for the standing mandate arrangement with coerced individuals as victims; public_health_primary authors low epsilon with refusers framed as cost-imposers; proportionality_reading authors threat-indexed epsilon. The members share no epsilon and no victim set, so they are separate files linked via affects_constraints rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__bodily_autonomy_primary, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
