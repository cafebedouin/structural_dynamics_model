% ============================================================================
% CONSTRAINT STORY: dignity_kernel__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__autonomy_rights_reading, []).

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
 *   constraint_id: dignity_kernel__autonomy_rights_reading
 *   human_readable: Autonomy-Rights Grounding of Human Dignity (vs. Divine Image)
 *   domain: theological/philosophical/technological governance
 *
 * SUMMARY:
 *   This constraint is one reading of a contested kernel about the ground of
 *   human dignity. The autonomy-rights reading asserts that dignity derives
 *   from human autonomy, rationality, and capacity for informed consent—not
 *   from divine image or metaphysical essence. This reading authorizes
 *   secular governance of AI, algorithmic systems, and technology: if
 *   autonomy is dignity's ground, then opacity and coercion violate dignity,
 *   and transparency and consent become requirements. The constraint is a
 *   TANGLED ROPE: it solves a real coordination problem (enabling
 *   cross-cultural, rights-based governance without religious consensus)
 *   while extracting asymmetric costs from those whose autonomy is actually
 *   most constrained and who lack standing within the framework itself. The
 *   measurement series tracks how suppression and theater have intensified
 *   over 60 years even as the reading became institutionally dominant,
 *   revealing a divergence between the reading's legitimacy claims and its
 *   actual operation.
 *
 * KEY AGENTS:
 *   - Secular rights institutions (UN, ILO, EU): set the agenda, enforce autonomy-rights framing, benefit from institutional authority
 *   - Workers subject to opaque algorithms (gig workers, warehouse staff): bear costs of algorithmic control framed as 'efficiency'; unable to contest systems in terms the framework recognizes
 *   - Marginalized populations (incarcerated, migrants, disabled): identity-locked into systems that deny autonomy while claiming to protect dignity; the framework's exclusion mechanisms are internal to the reading itself
 *   - Transparency advocates (civil society, researchers): benefit from the reading's regulatory mandates; elevate their institutional epistemic authority
 *   - Tech companies (Amazon, Uber, Meta, OpenAI): pay compliance costs; arbitrage regulatory gaps; develop 'autonomy theater' to maintain control
 *   - Imago dei traditionalists (excluded): argue dignity is equal in all persons prior to any capability; reading foreclosed them from governance conversations
 *   - Posthumanist enhancement advocates (excluded): argue autonomy should ground dignity regardless of substrate; reading limits dignity to human-level rationality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, 0.62).
domain_priors:suppression_score(dignity_kernel__autonomy_rights_reading, 0.51).
domain_priors:theater_ratio(dignity_kernel__autonomy_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__autonomy_rights_reading, "Autonomy-Rights Grounding of Human Dignity (vs. Divine Image)").
narrative_ontology:topic_domain(dignity_kernel__autonomy_rights_reading, "theological/philosophical/technological governance").

domain_priors:requires_active_enforcement(dignity_kernel__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__autonomy_rights_reading, 'e7f54225-45e5-44f0-b7f1-6c564d557dfd').
narrative_ontology:cs_kernel_codification('e7f54225-45e5-44f0-b7f1-6c564d557dfd', fixed_text).
narrative_ontology:cs_authority_grounding('e7f54225-45e5-44f0-b7f1-6c564d557dfd', extraction).
narrative_ontology:cs_interpretation_layer_present('e7f54225-45e5-44f0-b7f1-6c564d557dfd').
narrative_ontology:cs_reading_relation('e7f54225-45e5-44f0-b7f1-6c564d557dfd', dignity_kernel__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7f54225-45e5-44f0-b7f1-6c564d557dfd', dignity_kernel__posthumanist_reading, coexists_with).
narrative_ontology:cs_axiom('e7f54225-45e5-44f0-b7f1-6c564d557dfd', foundational, autonomy_as_dignity_ground).
narrative_ontology:cs_axiom_status(autonomy_as_dignity_ground, holdable).
narrative_ontology:cs_axiom_grounding('e7f54225-45e5-44f0-b7f1-6c564d557dfd', autonomy_as_dignity_ground, deontological).
narrative_ontology:cs_axiom('e7f54225-45e5-44f0-b7f1-6c564d557dfd', foundational, secular_rational_agency_sufficiency).
narrative_ontology:cs_axiom_status(secular_rational_agency_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('e7f54225-45e5-44f0-b7f1-6c564d557dfd', secular_rational_agency_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('e7f54225-45e5-44f0-b7f1-6c564d557dfd', autonomous_rational_agent_standard).
narrative_ontology:cs_drift_state('e7f54225-45e5-44f0-b7f1-6c564d557dfd', contemporary_algorithmic_opacity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7f54225-45e5-44f0-b7f1-6c564d557dfd', '').
narrative_ontology:cs_kernel_id(dignity_kernel__autonomy_rights_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, secular_rights_institutions).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, transparency_advocates).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, labor_protection_regimes).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, workers_subject_to_opaque_algorithms).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, marginalized_populations_denied_autonomy).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, surveillance_capitalism_subjects).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__autonomy_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dignity_kernel__autonomy_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__autonomy_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignity_kernel__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 because the constraint transfers authority from those most likely to experience autonomy-violation (workers, marginalized populations) to those who can articulate rights-claims within the framework (institutions, transparency advocates, legal actors). The measurement series shows extractiveness RISING from 0.28 (1965, early codification) to 0.62 (2026, mature institutional dominance): as the reading became authoritative, its coordination function (cross-cultural governance) solidified, but so did the gap between rights-talk and actual autonomy outcomes. Suppression is measured at 0.51 because the reading requires active institutional exclusion of alternative dignity-groundings (imago dei, posthumanism) to maintain its framework; this suppression is not coercive in a crude sense, but it operates through denial of standing and reframing of excluded claims as pre-autonomy or post-autonomy rather than autonomous. Theater (0.28) reflects the gap between transparency machinery and actual autonomy: workers see consent screens and data-access dashboards while their schedules and task allocation remain opaque; marginalized populations gain nominal rights while being subject to algorithmic systems that violate those rights with institutional sanction. The theater_ratio has risen steadily (0.08 in 1965 to 0.28 in 2026) because the machinery for performing autonomy-protection has become more elaborate even as actual autonomy outcomes for the most constrained populations have stagnated or declined. This divergence is the signature of a tangled-rope constraint that is extractive precisely through its appearance of non-extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats will compute radically different types from the same structural data. From the secular-rights-institution seat, the constraint is a Rope: it solves genuine coordination (cross-cultural dignity governance without theological consensus); suppression and theater are understood as costs of maintaining the framework against retrograde forces (religious traditionalism, technological nihilism). From the worker or marginalized-population seat, the same constraint is a Snare: their autonomy is systematically violated by opaque systems; the rights-machinery that claims to protect them is inaccessible (lack of standing, inability to contest algorithmic decisions in autonomy-terms) or actively complicit (systems that deny autonomy are justified through autonomy-protection rationales). From the imago-dei-traditionalist seat (structurally excluded), the constraint appears as Pure Extraction: it is designed to eliminate theistic dignity-grounding from governance, not for genuine coordination but to concentrate authority in secular institutions. The engine will compute these divergent types from the power, exit, and beneficiary/victim data; the authored claim (tangled_rope) reflects the structural fact that coordination (secular cross-cultural governance) and extraction (authority transfer, marginalization of actual autonomy-violations) are entangled in the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: (1) secular_rights_institutions (d ≈ 0.15, full beneficiary end): set the agenda, enforce the reading, derive institutional authority and jurisdictional scope. High power, analytical exit. (2) transparency_advocates (d ≈ 0.25, near-beneficiary): benefit from regulatory mandate for transparency; their expertise becomes valuable; organized power, mobile exit allows them to migrate between institutional seats. (3) labor_protection_regimes (d ≈ 0.30, moderate beneficiary): gain authority to challenge algorithmic management; institutional power; analytical exit (can always reformulate labor law). Victims: (1) workers_subject_to_opaque_algorithms (d ≈ 0.88, near-full-target): pay compliance costs without gaining autonomy; trapped exit; powerless; immediate horizon (cannot wait for systemic change). (2) marginalized_populations_denied_autonomy (d ≈ 0.92, near-full-target): systematically denied the autonomy the reading claims to protect; identity-locked exit (the reading's denial of their autonomy is constitutive of their marginalization); powerless; biographical horizon. Tech companies (d ≈ 0.62, moderate target/beneficiary): pay compliance costs (engineering labor, liability) but arbitrage them through performance and regulatory arbitrage; institutional power, arbitrage exit. The engine will derive these from the beneficiary/victim declarations and exit modulation; the directionality logic here documents the structural reasoning that justifies the beneficiary/victim splits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-modern dignity frameworks could not accommodate pluralism or universal rights without theological consensus) appears RESOLVED via the autonomy-rights reading at t=1965–1985 (early institutional codification, founding_problem_status=live, coordination function genuine). By t=2015–2026, the founding problem status becomes CONTESTED: the reading SOLVED pluralism-in-governance (UN, international law, cross-cultural rights-talk work), but it CREATED a new problem that it cannot address: actual autonomy-violations of the most constrained populations. The measurement series tracks this mandatrophy: extractiveness rises (0.28→0.62) as the reading becomes dominant; theater rises (0.08→0.28) as machinery for performing autonomy-protection decouples from actual autonomy outcomes; suppression stabilizes (0.15→0.51→0.51) as institutional exclusion of alternatives becomes normalized. The constraint does NOT meet the definition of Scaffold (no sunset clause; the reading has hardened, not faded). It is a Tangled Rope precisely because: (1) it coordinates genuine governance problem (cross-cultural rights adjudication without theological consensus); (2) it asymmetrically extracts authority from alternative dignity-grounds and from the actual autonomy-experiences of marginalized populations; (3) both functions are carried by the same mechanism (the autonomy-rationality standard that enables secular universalism also enables exclusion of those whose autonomy has been violated or whose rationality is not yet/currently/presumed-not-to-be the relevant kind). The mandatrophy-resolved verdict is NEGATIVE: the founding problem has not been superseded; it has been replaced by a new problem (how to maintain the reading's legitimacy while it systematically violates the autonomy it claims to protect) that the reading is structurally incapable of addressing without abandoning its core premise. The extraction (0.62) persists because the reading cannot acknowledge the asymmetry without losing its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ground_of_dignity_empirical,
    'Is the autonomy-rights reading an accurate empirical description of what dignity actually IS (a metaphysical property), or is it a normative choice about what we will treat as dignity for purposes of governance?',
    'Philosophical analysis of the grounding premises: if dignity is a discovered fact about human persons, the reading is descriptive; if it is a legislative/institutional choice about what will be legally protected, it is normative choice masquerading as metaphysics.',
    'If the reading is normative choice, its exclusion of imago-dei and posthumanist dignity-groundings is a structural decision, not a logical consequence — the exclusion becomes a straightforwardly extractive act. If the reading is descriptive, the exclusion can be defended as truth-tracking rather than power-consolidation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ground_of_dignity_empirical, conceptual, 'Whether the autonomy-rights ground for dignity is discovered or legislated.').

omega_variable(
    autonomy_as_sufficient_dignity_ground,
    'Is human autonomy (understood as rational capacity for informed consent) sufficient to ground dignity in all persons who lack it, or are those without such autonomy excluded from the dignity framework?',
    'Examination of actual court and administrative decisions: how are infants, severely cognitively disabled people, people with dementia, and AI systems treated in rights adjudication under the autonomy-rights frame? Are they protected, and if so, on what secondary grounds?',
    'If autonomy is NOT sufficient (other grounds are invoked for those lacking rationality), the reading has a hidden imago-dei or capability-independent element and is not purely autonomy-grounded. If autonomy IS sufficient but is attributed fictively or paternalistically to those lacking it, the reading relies on theater. If some people lack dignity-protection entirely, the reading is explicitly exclusive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_as_sufficient_dignity_ground, empirical, 'Whether the autonomy-rationality standard consistently determines who has dignity-protection.').

omega_variable(
    identity_lock_suppression_mechanism,
    'For marginalized populations (incarcerated, migrant, disabled), is the suppression of alternatives to the autonomy-rights reading structural (external barriers prevent hearing imago-dei arguments) or internalized (the reading has become part of the victim''s self-understanding, making exit unthinkable)?',
    'Post-constraint-exit trajectory: if marginalized populations retain autonomy-framing even after exposure to alternative dignity-grounds (imago dei, posthumanism), the suppression is internalized. If they recover capacity to use alternative frameworks once external barriers are removed, suppression is structural.',
    'If internalized, the effective suppression is higher than the measured 0.51 — the reading has colonized the interior thoughts of those it constrains. If structural, institutional redesign could enable exit. The distinction matters for mandatrophy: internalized suppression is extractive in a more profound way because it prevents the constrained from even recognizing their constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Structural vs. internalized suppression of alternative dignity-groundings.').

omega_variable(
    alternative_dignity_kernel_viability,
    'Could a dignity framework that COEXISTS with the autonomy-rights reading (rather than excluding it) solve the coordination problem the autonomy-rights reading addresses without asymmetrically extracting authority from alternative grounds?',
    'Natural experiment: jurisdictions or institutions that adopt explicitly pluralist dignity frameworks (acknowledging autonomy, imago dei, and posthumanist dignity-grounds as legitimate simultaneously) and measuring governance coherence, rights-protection outcomes, and institutional authority distribution.',
    'If a pluralist framework could coordinate governance while respecting alternative dignity-grounds, the autonomy-rights reading''s exclusion of alternatives is revealed as unnecessary for coordination and becomes purely extractive (authority consolidation in secular institutions). If pluralism fragments governance, the exclusion is necessary for coordination (the extraction is a coordination cost).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_dignity_kernel_viability, empirical, 'Whether plurality of dignity-grounds is governable without exclusion.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is the distinction between the autonomy_rights_reading and the imago_dei_reading a difference in what dignity fundamentally IS, or is it a difference in INSTITUTIONAL AUTHORITY STRUCTURE (secular vs. religious bodies deciding dignity-meaning) that has been packaged as a metaphysical dispute?',
    'Historical genealogy of the reading''s adoption: if the shift from imago-dei to autonomy-rights reasoning happened contemporaneously with the transfer of dignity-adjudication authority from religious to secular institutions, the reading is partly an epiphenomenon of institutional reorganization, not a philosophical discovery.',
    'If the reading is epiphenomenal to institutional power-shift, then explicitly acknowledging the shift (rather than pretending the reading is truth-driven) would clarify what extraction is actually occurring. If the reading preceded the institutional shift, it is more defensible as truth-tracking rather than power-consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether the kernel reading is a metaphysical claim or an institutional-authority claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__autonomy_rights_reading, 1965, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1965, dignity_kernel__autonomy_rights_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(dign_tr_t1985, dignity_kernel__autonomy_rights_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(dign_tr_t2005, dignity_kernel__autonomy_rights_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(dign_tr_t2015, dignity_kernel__autonomy_rights_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(dign_tr_t2023, dignity_kernel__autonomy_rights_reading, theater_ratio, 2023, 0.27).
narrative_ontology:measurement(dign_tr_t2026, dignity_kernel__autonomy_rights_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(dign_be_t1965, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(dign_be_t1985, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement(dign_be_t2005, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement(dign_be_t2015, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(dign_be_t2023, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2023, 0.61).
narrative_ontology:measurement(dign_be_t2026, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1965, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1965, 0.15).
narrative_ontology:measurement(dign_su_t1985, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(dign_su_t2005, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2005, 0.41).
narrative_ontology:measurement(dign_su_t2015, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(dign_su_t2023, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2023, 0.51).
narrative_ontology:measurement(dign_su_t2026, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2026, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__autonomy_rights_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignity_kernel__autonomy_rights_reading, 0.12).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__imago_dei_reading).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__posthumanist_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the dignity_kernel. The autonomy_rights_reading structures AI governance, algorithmic transparency, and labor protection through secular autonomy metrics. Two sibling readings (imago_dei and posthumanist) instantiate alternative dignity-grounds that coexist with but are structurally excluded from this reading's governance authority. All three stories are linked via affects_constraints; the constraint family decomposes the unified concept of 'human dignity' into three structurally distinct claims with different ε values and different beneficiary/victim structures. The decomposition is necessary because the observable (how to ground dignity in governance) yields different extractiveness scores depending on which reading's assumptions are adopted: measuring dignity-protection through autonomy-metrics gives 0.62 extractiveness; measuring through imago-dei metrics would yield different structural extraction (religious authority exclusion); measuring through posthumanist metrics would reveal exclusion of enhanced/AI entities. The family is complete and mutually constraining: none can be understood in isolation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignity_kernel__autonomy_rights_reading, powerless, 0.92).
constraint_indexing:directionality_override(dignity_kernel__autonomy_rights_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
