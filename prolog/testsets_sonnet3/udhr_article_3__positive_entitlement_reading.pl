% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 — Positive Entitlement Reading (State-Provided Material Security)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the positive-entitlement reading of the UDHR
 *   Article 3 kernel: that the right to 'life, liberty and security of
 *   person' obligates affirmative state provision of the material conditions
 *   — welfare, healthcare, housing — necessary to make life and security real
 *   rather than formal. This is a genuinely distinct constraint from the
 *   negative-liberty reading (which reads Article 3 as a prohibition on state
 *   violence) and the procedural-hybrid reading (which reads it as
 *   due-process guarantees only). Under this reading, the coordination
 *   function (pooling risk against poverty and illness) is real, but it is
 *   bundled with an asymmetric extraction structure: high-net-worth
 *   taxpayers, property owners, employers, and — in its expanded form —
 *   restricted speakers bear costs that fund benefits collected by a
 *   different population, administered by a bureaucracy with its own
 *   institutional interest in the arrangement's continuation and growth.
 *
 * KEY AGENTS:
 *   - low_income_households: primary beneficiary (powerless/trapped) — receives welfare transfer
 *   - welfare_administering_agencies: agenda-setter (institutional/analytical) — designs and enforces provision, expands mandate
 *   - high_net_worth_taxpayers: primary payer (powerful/mobile) — funds transfer, has partial exit via jurisdiction
 *   - speech_restricted_dissenters: secondary payer (powerless/trapped) — bears the expression-rights cost of the reading's expanded security logic
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates the reading's boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.58).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 — Positive Entitlement Reading (State-Provided Material Security)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '18263362-1a03-4681-98a2-97421feb737f').
narrative_ontology:cs_kernel_codification('18263362-1a03-4681-98a2-97421feb737f', fixed_text).
narrative_ontology:cs_authority_grounding('18263362-1a03-4681-98a2-97421feb737f', distributed).
narrative_ontology:cs_reading_relation('18263362-1a03-4681-98a2-97421feb737f', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('18263362-1a03-4681-98a2-97421feb737f', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('18263362-1a03-4681-98a2-97421feb737f', foundational, material_deprivation_negates_formal_liberty).
narrative_ontology:cs_axiom_status(material_deprivation_negates_formal_liberty, holdable).
narrative_ontology:cs_axiom_grounding('18263362-1a03-4681-98a2-97421feb737f', material_deprivation_negates_formal_liberty, deontological).
narrative_ontology:cs_axiom('18263362-1a03-4681-98a2-97421feb737f', foundational, state_affirmative_duty_to_provide_subsistence).
narrative_ontology:cs_axiom_status(state_affirmative_duty_to_provide_subsistence, holdable).
narrative_ontology:cs_axiom_grounding('18263362-1a03-4681-98a2-97421feb737f', state_affirmative_duty_to_provide_subsistence, instrumental).
narrative_ontology:cs_reference_frame('18263362-1a03-4681-98a2-97421feb737f', post_war_social_democratic_settlement).
narrative_ontology:cs_drift_state('18263362-1a03-4681-98a2-97421feb737f', contemporary_austerity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('18263362-1a03-4681-98a2-97421feb737f', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, low_income_households).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, uninsured_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, unhoused_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, welfare_administering_agencies).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, high_net_worth_taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_owners_subject_to_redistribution).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, employers_subject_to_mandated_contributions).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, speech_restricted_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive welfare transfers, subsidized healthcare, and housing assistance funded through the redistributive apparatus this reading requires the state to build and maintain. Their material security depends on the continued operation and funding of these programs; they have no exit from needing them and little power to guarantee their continuation independent of political will.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, low_income_households, beneficiary,
    powerless, biographical, trapped, national).

% Gain access to healthcare only through state-guaranteed provision under this reading of Article 3. Without market alternatives they are structurally dependent on the entitlement being honored; if it lapses they have no private fallback.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, uninsured_populations, beneficiary,
    powerless, biographical, trapped, national).

% Depend on housing guarantees derived from this reading for shelter. Their situation is the clearest test case for whether the entitlement is real or aspirational — provision or non-provision is immediately life-affecting.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, unhoused_populations, beneficiary,
    powerless, immediate, trapped, national).

% Design, fund, and enforce the entitlement programs this reading requires — set eligibility rules, collect the taxes/contributions that fund provision, and adjudicate compliance. They administer the transfer and expand their own institutional mandate and budget by doing so.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, welfare_administering_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Bear a disproportionate share of the taxation funding welfare, healthcare, and housing guarantees. Some have exit via capital mobility or jurisdictional relocation; those who stay pay the transfer directly with no service received in proportion to contribution.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, high_net_worth_taxpayers, payer,
    powerful, biographical, mobile, national).

% Face property taxation, rent controls, or compelled land-use changes justified by the state's housing-provision obligation. Their exit options are limited by immobility of real property; they experience the entitlement as a direct claim on assets they hold.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_owners_subject_to_redistribution, payer,
    moderate, biographical, constrained, national).

% Pay mandated healthcare or welfare contributions on behalf of employees as part of the state's provision scheme. Smaller employers experience this as a fixed cost with no market alternative; larger firms can partially offshore or automate around it.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, employers_subject_to_mandated_contributions, payer,
    moderate, biographical, constrained, national).

% Face hate-speech and incitement restrictions justified as protecting vulnerable groups' security interests under this reading's expanded conception of Article 3. They experience the entitlement's protective logic as a direct constraint on what they may say, with no institutional forum reliably available to contest the restriction's scope.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, speech_restricted_dissenters, payer,
    powerless, biographical, trapped, national).

% Adjudicate disputes between the entitlement claims of beneficiaries and the property/speech claims of payers, deciding how far the positive-entitlement reading extends and where it yields to competing rights. Their rulings determine whether the reading expands, stabilizes, or contracts.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools resources across a national population so that individuals facing illness, poverty, or homelessness receive a guaranteed material floor rather than bearing catastrophic risk alone — a genuine collective-action solution to the problem that market provision alone leaves a share of the population without life-sustaining resources.
% TRANSFER_FUNCTION: Moves tax revenue, mandated contributions, and in some readings expression rights from higher-income taxpayers, property owners, and employers to low-income, uninsured, and unhoused populations, administered and expanded by the welfare bureaucracy that collects and disburses it.
% ABSENT_VOICES: Future taxpayers who will bear the fiscal liabilities created by open-ended entitlement commitments are not present in current distributional disputes. Minority political viewpoints restricted under expanded hate-speech doctrine are structurally excluded from the forums where the doctrine's scope is set, since the restriction itself narrows their access to the public conversation about its own limits.
% DISAPPEARANCE_RATIONALE: Beneficiaries would say the world rearranges catastrophically — welfare, healthcare, and housing guarantees would evaporate, producing immediate material harm. Payers and free-expression advocates would say the world substantially improves — taxation and speech restriction pressure would lift. The disagreement over whether disappearance is catastrophic or liberating is itself constitutive of the underlying kernel contest; it does not resolve within this reading alone.
% FOUNDING_PROBLEM: Post-WWII drafters sought to prevent the kind of mass immiseration, and the political radicalization it fuels, that preceded totalitarian capture in interwar Europe — the view that formal liberty without material security is hollow and politically unstable.
% FOUNDING_PROBLEM_CORROBORATION: Social-democratic welfare states and international human rights bodies (ICESCR committee, UN Special Rapporteurs) attest the founding problem remains live — material deprivation persists and destabilizes democracies. Libertarian and originalist constitutional scholars, writing from outside the beneficiary coalition, attest that the founding problem as originally understood (preventing state violence and arbitrary deprivation of life) has been substantively redefined by this reading rather than fulfilled, and that the redefinition itself is the contested claim.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, contested).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 by 2024 because the entitlement structure has expanded over 75 years from a narrow post-war floor into a broad redistributive and speech-restrictive regime whose costs are increasingly decoupled from any bounded insurance logic — the transfer grows independent of a stable, agreed-upon threshold of 'necessary for life and security.' Suppression (0.58) reflects the active enforcement required to compel contributions and restrict speech; it is lower than extraction because much compliance is achieved through normalized tax administration rather than direct coercion. Theater ratio is modest (0.28) — the coordination function (actual welfare, healthcare, housing delivery) is substantially real, not merely performative, which is why this reading is authored as tangled_rope rather than snare.
 *
 * PERSPECTIVAL GAP:
 *   From the welfare-agency and beneficiary seats, this reading computes as coordination — a genuine collective solution to material insecurity that formal liberty alone cannot solve. From the taxpayer, property-owner, and dissenter seats, the same structure computes as extraction backed by active enforcement (tax compliance apparatus, hate-speech adjudication) with no meaningful exit. The engine should register this seat divergence directly from the structural data; the claimed_type of tangled_rope is authored because both a genuine coordination function AND asymmetric extraction requiring active enforcement are simultaneously present — this is not resolved by declaring one seat's perception canonical.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income, uninsured, and unhoused populations are structural beneficiaries with low derived d — the entitlement subsidizes them directly and their exit options (trapped) reinforce rather than offset this. Welfare administering agencies sit near the beneficiary end structurally despite institutional power, because their mandate and budget grow with the entitlement's scope. High-net-worth taxpayers, property owners, and employers sit toward the target end — they fund the transfer without proportional benefit, though taxpayer mobility (arbitrage-adjacent exit) partially damps their derived d relative to property owners, whose asset immobility keeps them nearer full-target. Speech-restricted dissenters are a distinct victim class: their cost is not fiscal but expressive, and their trapped exit options place them near full-target despite low institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war material immiseration destabilizing democracy) remains contested as live or historically superseded — this is precisely the mandatrophy question the R5 interview is built to surface. If the founding problem is judged dead in wealthy welfare states with mature safety nets, but the entitlement apparatus has instead grown in scope (extending into speech restriction and broader redistribution), that is a classic mandatrophy signature: the original coordination justification has weakened while the extraction machinery built to serve it has expanded. The corroboration split — international rights bodies say the problem is live; originalist/libertarian scholars outside the beneficiary coalition say it has been redefined rather than solved — is the exact kind of outside-corroboration test the schema requires, and it does not resolve cleanly, which is why founding_problem_status is authored as contested rather than either live or dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the UDHR Article 3 text (''right to life, liberty and security of person'') itself select among the positive-entitlement, negative-liberty, and procedural-hybrid readings, or is the text genuinely underdetermined such that the reading is supplied by the interpreting body''s prior commitments?',
    'Comparative analysis of drafting history (travaux préparatoires), and cross-jurisdictional convergence/divergence in how domestic courts and treaty bodies interpret parallel language in ICCPR/ICESCR — if interpretive practice converges despite ideological variation, the text is more determinate than it appears.',
    'If the text is genuinely underdetermined, all three readings are equally legitimate constructions and the classification differences across the three constraint files reflect differences in what each reading''s proponents choose to build, not differences in fidelity to a shared source. If the text selects one reading, the other two readings should be understood as departures rather than siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether Article 3''s text determines a reading or merely permits this reading among others.').

omega_variable(
    entitlement_scope_boundary,
    'Is there a principled, non-arbitrary boundary on what material provision is ''necessary for life and security'' under this reading, or does the entitlement''s scope necessarily expand without a stopping point once the positive-obligation frame is accepted?',
    'Track whether jurisdictions that adopt the positive-entitlement reading (e.g., South Africa, India via directive principles) show convergence toward a stable floor over decades, or continued expansion of what counts as constitutionally mandated provision.',
    'A stable floor supports classifying this as a bounded coordination mechanism (closer to rope); continued expansion without a stopping point supports the tangled_rope or drift-toward-snare reading, since extraction would be growing independent of any agreed coordination target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entitlement_scope_boundary, empirical, 'Whether the entitlement has a principled scope limit or expands without bound.').

omega_variable(
    speech_restriction_necessity,
    'Is restricting hate speech genuinely necessary to secure vulnerable groups'' Article 3 security interests, or is the speech-restriction component a separable extraction riding on the entitlement reading''s coordination legitimacy?',
    'Comparative study of security outcomes for vulnerable groups in jurisdictions with strong hate-speech restriction versus jurisdictions with strong entitlement provision but weak speech restriction, holding other variables constant.',
    'If separable, the speech-restriction component of this reading should potentially be decomposed into its own constraint file per the ε-invariance principle, since it may carry a materially different ε and victim structure than the welfare/healthcare/housing component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speech_restriction_necessity, conceptual, 'Whether speech restriction is intrinsic to or separable from the material-provision obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(udhr_tr_t1965, udhr_article_3__positive_entitlement_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(udhr_tr_t1980, udhr_article_3__positive_entitlement_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement(udhr_tr_t1995, udhr_article_3__positive_entitlement_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(udhr_tr_t2010, udhr_article_3__positive_entitlement_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__positive_entitlement_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(udhr_be_t1965, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(udhr_be_t1980, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(udhr_be_t1995, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(udhr_be_t2010, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(udhr_su_t1965, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(udhr_su_t1980, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement(udhr_su_t1995, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(udhr_su_t2010, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(udhr_article_3__positive_entitlement_reading, 0.15).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3_negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3_procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the udhr_article_3 kernel. negative_liberty_reading claims a narrow, low-ε prohibition on state violence with no redistributive beneficiary/victim structure. procedural_hybrid_reading claims a thin, largely uncontested due-process guarantee. This file (positive_entitlement_reading) claims the broadest, highest-ε reading, with an explicit beneficiary class (vulnerable/low-income groups via state action) and victim class (property/expression rights holders). Per the ε-invariance principle, these are three distinct constraints sharing a textual source, not one constraint measured three ways — each carries its own stable ε and its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
