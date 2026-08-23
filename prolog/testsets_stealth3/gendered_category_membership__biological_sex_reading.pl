% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Biological-Marker Criterion for Gendered Category Membership
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   The arrangement under contest defines membership in the gendered
 *   categories ('woman', 'man') by immutable biological markers — chromosomes
 *   and reproductive anatomy observed at birth — and routes every
 *   sex-segregated allocation through that definition. The reading presents
 *   the criterion as a fact of nature rather than a chosen rule, but the
 *   boundary is actively maintained: statutory codification, civil-registry
 *   practice, eligibility testing in sport, and space-access challenges all
 *   defend it against self-declaration alternatives that are visibly
 *   operational in peer jurisdictions. KEY AGENTS (by structural
 *   relationship): - transgender_women: primary target
 *   (powerless/identity_locked) — bears the recognition and access costs of
 *   the definition - transgender_men: primary target
 *   (powerless/identity_locked) — reverse-mismatched into the wrong
 *   allocations - nonbinary_persons: silent target
 *   (powerless/identity_locked) — offered no category at all -
 *   intersex_persons: silent target (powerless/trapped) — classified by
 *   administrative fiat attached to their bodies -
 *   cis_women_under_biological_definition: positioned beneficiary
 *   (organized/constrained) — receives the protected side of every
 *   segregation - legislative_policy_bodies: agenda setter
 *   (institutional/arbitrage) — writes, tightens, or rewrites the criterion -
 *   womens_sports_governing_bodies: agenda setter (institutional/constrained)
 *   — administers eligibility without collecting from it -
 *   gender_critical_advocacy_organizations: secondary collector
 *   (organized/mobile) — organizes around the boundary's continued contest -
 *   international_human_rights_bodies: analytical observer
 *   (institutional/analytical) — reviews without enforcing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.72).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.7).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological-Marker Criterion for Gendered Category Membership").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, 'bdf7aeb4-f113-4a4f-94f6-e3793afa960d').
narrative_ontology:cs_kernel_codification('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', formalized).
narrative_ontology:cs_authority_grounding('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', expertise).
narrative_ontology:cs_interpretation_layer_present('bdf7aeb4-f113-4a4f-94f6-e3793afa960d').
narrative_ontology:cs_reading_relation('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', gendered_category_membership__social_role_reading, forecloses).
narrative_ontology:cs_axiom('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', foundational, membership_constituted_by_immutable_birth_biology).
narrative_ontology:cs_axiom_status(membership_constituted_by_immutable_birth_biology, holdable).
narrative_ontology:cs_axiom_grounding('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', membership_constituted_by_immutable_birth_biology, empirically_contingent).
narrative_ontology:cs_axiom('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', secondary, segregation_function_requires_fixed_boundary).
narrative_ontology:cs_axiom_status(segregation_function_requires_fixed_boundary, holdable).
narrative_ontology:cs_axiom_grounding('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', segregation_function_requires_fixed_boundary, instrumental).
narrative_ontology:cs_reference_frame('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', immutable_birth_binary_registration).
narrative_ontology:cs_drift_state('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', post_self_id_legislation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bdf7aeb4-f113-4a4f-94f6-e3793afa960d', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women_under_biological_definition).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, gender_critical_advocacy_organizations).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_men).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, nonbinary_persons).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adult women whose membership in the category and whose access to women's refuges, prison placements, hospital wards, changing rooms, and sporting categories rest on the birth-anatomy definition. They receive the protected side of every segregation the definition administers and bear none of its documentation or challenge burdens. The group is internally divided: many endorse the boundary, others favor identity-based access, and advocacy organizations speak for the endorsing portion in policy arenas.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women_under_biological_definition, beneficiary,
    organized, generational, constrained, global).

% Women whose identity and lived social position do not match their recorded sex at birth. Under the birth-anatomy definition they are placed outside the category they live in: they are barred or challenged in women's facilities, must carry documents that contradict their presentation, and face case-by-case scrutiny wherever access turns on the definition. Leaving that position would mean undoing their transition and disowning their identity, which they experience as self-erasure.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% Men whose recorded sex at birth is female. The definition classes them with women for facility and service allocation regardless of presentation or, where they exist, corrected legal markers — producing reverse mismatches such as trans men directed into women's shelters and wards. Exit carries the same self-erasure cost as it does for trans women.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, transgender_men, payer,
    powerless, biographical, identity_locked, global).

% People whose gender is neither man nor woman. The binary definition offers them no correct entry at all: every registration, facility rule, and category assignment forces a misclassification in one direction or the other. Their objection has no seat in the arrangement the definition structures.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, nonbinary_persons, payer,
    powerless, biographical, identity_locked, global).

% People born with variations in sex characteristics that do not fit the two-category template cleanly. The definition resolves their case by administrative fiat at birth — historically often accompanied by nonconsensual infancy normalization procedures — and the assignment follows them through every later document. They cannot relocate out of the classification; it attaches to their bodies.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_persons, payer,
    powerless, biographical, trapped, global).

% National and subnational legislatures and civil-registration authorities. They enact the statutory definitions, direct how sex is recorded and amended on documents, and decide whether access rules turn on birth anatomy or declared identity. They can rewrite the criterion at will but face intense organized pressure from both directions; recent sessions in several jurisdictions have produced codifications that tighten the biological test.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, legislative_policy_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Federations that set eligibility for the female category. They design and revise testing and documentation regimes intended to keep competition fair and safe, and absorb litigation and member-federation pressure whichever direction eligibility moves. They administer the boundary inside their domain but collect nothing from it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, womens_sports_governing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Campaign groups organized around defending the birth-anatomy definition in law and policy. They draft model legislation, litigate, and lobby across jurisdictions. The ongoing contest supplies their funding, membership, and media standing, so their organizational continuity depends on the boundary remaining disputed.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_critical_advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% Treaty monitoring bodies and commissioners that review national regimes against rights frameworks covering private life, non-discrimination, and legal recognition. They issue findings and recommendations, compile jurisdictional comparisons, and hold no direct enforcement power over any national definition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, cis_women_under_biological_definition).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively checkable criterion for allocating access to sex-segregated provisions — refuges, custodial placements, medical wards, changing facilities, and the female sporting category — solving, once and centrally, the problem of who belongs where where physical segregation by sex is practiced.
% TRANSFER_FUNCTION: Moves official recognition and corresponding access away from transgender, nonbinary, and intersex persons toward preservation of unshared access for persons classified at birth; additionally moves the enforcement burden — documentation maintenance, challenge exposure, and litigation cost — onto the excluded.
% ABSENT_VOICES: Transgender, nonbinary, and intersex persons were absent from every historical moment at which the definition was codified; the categories were drawn over them without them. Women who dissent from the boundary are also spoken for rather than seated, since advocacy organizations claim the protecting-women voice wholesale.
% DISAPPEARANCE_RATIONALE: If the birth-anatomy criterion vanished overnight, every sex-segregated allocation would need a replacement rule immediately — custodial placement, refuge admission, ward assignment, and sporting eligibility would all have to be re-decided, registries would face amendment demands at scale, and the advocacy ecosystem organized around defending the criterion would lose its object. The arrangements built on the definition depend on it.
% FOUNDING_PROBLEM: Reliable classification of every person into one of two sexes for record-keeping, medicine, and the allocation of sex-segregated provisions — built when binary classification was assumed exhaustive, immutable, and uncontested.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration for the surviving core of the founding problem (sex-stratified medical and statistical data) comes from outside the benefiting parties: public-health and epidemiological practice continues to stratify by natal sex for clinical reasons. Corroboration that the categorical-exclusivity application remains necessary comes almost entirely from within the benefiting coalition; trans-led organizations, intersex advocacy, and international human rights bodies attest from outside that the exclusivity function is obsolete and harmful. No neutral attestation supports the exclusivity framing.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end): the definition's costs concentrate on people who cannot leave their identities, and the current legislative wave is widening them. Suppression (0.70, raw and unscaled — only extractiveness is scaled by the engine) reflects structural enforcement: statutory tests, document policing, space challenges. Theater is moderate (0.42): the segregated provisions genuinely function, but a growing share of activity is symbolic definitional combat — model bills, culture-war litigation, pledge campaigns — that defends the criterion rather than operating it. Accessibility_collapse is low (0.35) because the alternative criterion remains visibly live in neighboring jurisdictions; resistance is substantial (0.62) from rights bodies, parts of the medical profession, trans-led advocacy, and the dissenting portion of the beneficiary class itself. The temporal series runs on one shared grid (every metric authored at every point 0–56 by 8): extractiveness declines through the accommodation decades as medical-gated recognition spreads, bottoms near t=32, then climbs steeply as defensive codification ratchets. The escalation is cyclical in mechanism — incident, legislation, litigation, counter-mobilization — with each cycle resetting the baseline higher (intermittent-reinforcement dynamics); the endpoint values were captured during an active legislative cycle, i.e. at a late-cycle peak rather than a trough. Coalition note: the payer seats are individually powerless but their coalition capacity is real and partially realized, which is what keeps resistance at 0.62 rather than lower.
 *
 * PERSPECTIVAL GAP:
 *   From the positioned-beneficiary seat the arrangement is protective coordination: the boundary is what makes refuges, wards, and the female category mean anything. From the payer seats the identical structure operates as enforced erasure — the state declining to recognize the category one lives in, backed by documents and doorways. From the advocacy-organizational seat it is an organizing resource whose value peaks with contest intensity. The agenda-setter seats see administration, not gain. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: cis_women_under_biological_definition sits near the beneficiary pole (receives every protection, bears no enforcement cost, d near 0.0); the four payer groups sit near the full-target pole, pushed further by identity_locked and trapped exits (trapped or identity-locked targets sit nearer full-target than mobile ones — here the lock is literal, since exit means dissolving the identity the definition refuses). Legislative bodies derive near-symmetric (they administer without collecting). Gender-critical advocacy organizations derive low d through their beneficiary declaration even though what they collect is contest-resources rather than protected access. No directionality_overrides are needed: declarations plus exit options suffice, and overrides are keyed by power_atom, which would collide across the story's distinct institutional agents (legislature vs. federation vs. treaty body).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the arrangement as pure extraction would erase the genuine coordination work — sex-segregated refuges, custodial separation, and the female sporting category solve real problems that participants across every seat acknowledge. Reading it as pure coordination would erase the identity suppression that is the definition's principal modern product. The tangled_rope claim holds both halves: a live coordination function and an extraction layer riding on the same boundary, held together by active enforcement. Genealogically, the mandate bifurcates: the record-keeping core (natal-sex data for medicine) remains live and externally corroborated, while the exclusivity application is contested — hence founding_problem_status 'contested' rather than 'dead'. Because the status is contested rather than dead, the zombie/mismatch flag condition (dead status + world_rearranges verdict) does not fire; the persistence question stays open pending the separability omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the measured extraction belong to the birth-anatomy membership arrangement itself, or partly to instantiating the gendered_category_membership kernel through the biological reading rather than one of its siblings?',
    'Author the sibling-reading stories and compare victim sets, epsilon, and computed types across the family; divergence localized to the constitutive-criterion element distinguishes reading-artifact from arrangement-property.',
    'If extraction is partly a reading artifact, cross-reading comparisons using this story alone are invalid without family-level analysis; if arrangement-property, the biological reading''s constraint classifies independently of which reading a given jurisdiction adopts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame uncertainty: this story is one of three sibling readings of the membership kernel; sibling instantiation would shift the entire victim set.').

omega_variable(
    separability_of_segregation_from_exclusivity,
    'Can the coordination work of sex-segregated provision — refuges, custodial placement, wards, the female sporting category — be delivered under criteria other than immutable birth markers without degrading the functions participants rely on?',
    'Jurisdictional natural experiments: compare provision outcomes and integrity metrics in self-ID jurisdictions against matched biological-definition jurisdictions over a decade of operation.',
    'If the functions are separable, the definitional exclusivity layer is the extraction-bearing component and is remediable without touching provision; if inseparable, part of the measured cost is the price of the coordination itself and the tangled-rope balance shifts toward its rope half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_segregation_from_exclusivity, empirical, 'Whether the boundary''s coordination function and its exclusivity function come apart.').

omega_variable(
    dilution_harm_evidential_status,
    'Are the harms to cis women that justify the boundary — loss of single-sex provision, competitive unfairness — documented at the rate the boundary''s severity implies, or asserted?',
    'Systematic review of incident and outcome data from inclusive-access jurisdictions against the incidence rates cited in codification debates.',
    'Evidence thinner than asserted would reposition the boundary from protection toward purity-enforcement and raise effective extraction on the excluded; robust evidence would strengthen the arrangement''s coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dilution_harm_evidential_status, empirical, 'Evidential status of the category-dilution harm claim that positions the beneficiary class.').

omega_variable(
    enforcement_ratchet_direction,
    'Will the enforcement apparatus around the definition keep hardening — new statutes, documentation checks, eligibility testing — or decay as administrative practice drifts toward declared-identity handling?',
    'Track statutory amendments, litigation outcomes, and administrative practice over the coming decade; the suppression_requirement slope in this story''s measurement series is the leading indicator.',
    'Continued hardening pushes the arrangement toward the pure-extraction end of its family and dates any tangled-rope-to-snare transition; decay would return it toward ordinary coordination with residual contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_direction, empirical, 'Direction of the enforcement ratchet over the forward interval.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the suppression borne by the excluded groups is external — law, documents, space challenges — versus internalized: self-monitoring, concealment, and anticipatory withdrawal from spaces?',
    'Compare concealment and withdrawal indicators across recognition reforms: if they persist where rules relax, an internalized component is carrying part of the load.',
    'An internalized share means true suppression exceeds the structural measure and outlives formal reform — statutory fixes would under-treat the constraint, and the omega''s resolution would warrant revising the suppression figure upward post-reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__biological_sex_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__biological_sex_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__biological_sex_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(gend_tr_t32, gendered_category_membership__biological_sex_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(gend_tr_t40, gendered_category_membership__biological_sex_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(gend_tr_t48, gendered_category_membership__biological_sex_reading, theater_ratio, 48, 0.36).
narrative_ontology:measurement(gend_tr_t56, gendered_category_membership__biological_sex_reading, theater_ratio, 56, 0.42).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__biological_sex_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__biological_sex_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__biological_sex_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(gend_be_t32, gendered_category_membership__biological_sex_reading, base_extractiveness, 32, 0.44).
narrative_ontology:measurement(gend_be_t40, gendered_category_membership__biological_sex_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(gend_be_t48, gendered_category_membership__biological_sex_reading, base_extractiveness, 48, 0.62).
narrative_ontology:measurement(gend_be_t56, gendered_category_membership__biological_sex_reading, base_extractiveness, 56, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__biological_sex_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__biological_sex_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(gend_su_t24, gendered_category_membership__biological_sex_reading, suppression_requirement, 24, 0.26).
narrative_ontology:measurement(gend_su_t32, gendered_category_membership__biological_sex_reading, suppression_requirement, 32, 0.3).
narrative_ontology:measurement(gend_su_t40, gendered_category_membership__biological_sex_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(gend_su_t48, gendered_category_membership__biological_sex_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement(gend_su_t56, gendered_category_membership__biological_sex_reading, suppression_requirement, 56, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, social_role_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition: 'gendered_category_membership' is one contested commitment instantiated by three readings. This file authors the biological_sex_reading only. The sibling stories — gender_identity_reading (constituted by self-declaration) and social_role_reading (constituted by sustained performance and recognition) — carry their own epsilon, their own beneficiary/victim structure, and their own claimed types. The biological reading is the historically incumbent account (birth registration), from which the siblings diverge; edges run between all family members. Cross-reading comparison is family-level analysis, never a parameter inside any single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
