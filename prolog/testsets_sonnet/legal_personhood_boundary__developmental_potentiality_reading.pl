% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__developmental_potentiality_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Personhood-at-Conception Reading of the Legal Personhood Boundary
 *   domain: legal philosophy / constitutional law / rights theory
 *
 * SUMMARY:
 *   This story generates the developmental-potentiality reading of the legal
 *   personhood boundary kernel: the claim that personhood, and hence
 *   rights-bearing status, attaches at conception rather than at birth or at
 *   demonstrated cognitive capacity. This is one of three structurally
 *   distinct constraints sharing a natural-language label ('when do rights
 *   begin'); the other two — the restrictive anthropocentric reading
 *   (personhood limited to born humans with cognitive capacity) and the
 *   functional capacity reading (personhood tracks demonstrable cognitive
 *   capacity regardless of species) — are separate constraint stories with
 *   their own ε values, stakeholders, and classifications. This story does
 *   not describe or average over those readings; it instantiates only the
 *   conception-based claim and its structural consequences: fetal inclusion
 *   in the victim/beneficiary calculus, subordination of pregnant-person
 *   autonomy, and expanded state enforcement authority over pregnancy
 *   outcomes.
 *
 * KEY AGENTS:
 *   - pregnant_persons: primary target (moderate/trapped) — bears legal and medical consequences of the standard
 *   - reproductive_healthcare_providers: secondary target (moderate/constrained) — bears liability exposure for standard clinical practice
 *   - fetal_rights_advocacy_organizations: primary agenda-setter (organized/mobile) — authors and propagates the standard
 *   - state_prosecutorial_authorities: enforcement beneficiary (institutional/analytical) — gains new prosecutorial jurisdiction
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates the boundary dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.79).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Personhood-at-Conception Reading of the Legal Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal philosophy / constitutional law / rights theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '00a404ee-ac25-4d79-b043-62ca552dc882').
narrative_ontology:cs_kernel_codification('00a404ee-ac25-4d79-b043-62ca552dc882', distributed).
narrative_ontology:cs_authority_grounding('00a404ee-ac25-4d79-b043-62ca552dc882', distributed).
narrative_ontology:cs_reading_relation('00a404ee-ac25-4d79-b043-62ca552dc882', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('00a404ee-ac25-4d79-b043-62ca552dc882', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('00a404ee-ac25-4d79-b043-62ca552dc882', foundational, moral_status_attaches_at_biological_origin).
narrative_ontology:cs_axiom_status(moral_status_attaches_at_biological_origin, holdable).
narrative_ontology:cs_axiom_grounding('00a404ee-ac25-4d79-b043-62ca552dc882', moral_status_attaches_at_biological_origin, deontological).
narrative_ontology:cs_axiom('00a404ee-ac25-4d79-b043-62ca552dc882', secondary, developmental_continuity_grounds_rights_not_discontinuity).
narrative_ontology:cs_axiom_status(developmental_continuity_grounds_rights_not_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('00a404ee-ac25-4d79-b043-62ca552dc882', developmental_continuity_grounds_rights_not_discontinuity, conventional).
narrative_ontology:cs_reference_frame('00a404ee-ac25-4d79-b043-62ca552dc882', common_law_birth_threshold_baseline).
narrative_ontology:cs_drift_state('00a404ee-ac25-4d79-b043-62ca552dc882', post_roe_reversal_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('00a404ee-ac25-4d79-b043-62ca552dc882', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_prosecutorial_authorities).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, religious_institutions_with_conception_doctrine).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, reproductive_healthcare_providers).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, persons_seeking_ivf_or_fertility_treatment).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, human_life_trajectory_moral_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the full weight of the reading: pregnancy decisions, medical treatment during pregnancy, and even conduct before viability can be subject to legal scrutiny under a fetal-rights framework. Cannot exit the jurisdiction easily, cannot exit the pregnancy without confronting the personhood claim directly, and faces prosecution or civil liability for outcomes once framed as harm to a rights-bearer.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    moderate, biographical, trapped, national).

% Must practice under threat of civil or criminal liability for procedures that a conception-based personhood standard recasts as harming a second patient. Can relocate practice to other jurisdictions or exit the specialty, but at high professional and personal cost; standard medical judgment is subordinated to the constraint's rights claim.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, reproductive_healthcare_providers, payer,
    moderate, biographical, constrained, national).

% IVF routinely creates surplus embryos; a conception-based personhood standard treats each as a rights-bearer, exposing clinics and patients to liability for embryo disposal or storage decisions that were previously treated as ordinary medical practice. Exit means traveling to a jurisdiction without the standard, which is costly and not available to all.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, persons_seeking_ivf_or_fertility_treatment, payer,
    moderate, biographical, constrained, national).

% Author and lobby for the conception-based standard, frame it in legislatures and courts, and organize enforcement pressure (litigation, model statutes, referrals to prosecutors). Bears none of the pregnancy-specific costs and can shift strategy or venue as political conditions change.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Gains new jurisdiction to investigate and prosecute pregnancy outcomes once the fetus is a rights-bearer from conception — miscarriage, stillbirth, and substance use in pregnancy become potential predicates for prosecution. Administers and enforces the standard rather than bearing its costs.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_prosecutorial_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__developmental_potentiality_reading, state_prosecutorial_authorities, beneficiary).

% Sees a long-held theological premise (ensoulment or moral status at conception) vindicated in binding law, extending institutional doctrine's reach into state enforcement without the institution itself bearing any of the pregnancy-specific costs.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, religious_institutions_with_conception_doctrine, beneficiary,
    institutional, civilizational, analytical, national).

% Adjudicates disputes between the fetal-rights claim and autonomy or privacy claims, sets precedent on which reading of the personhood boundary controls in a given jurisdiction, and is the primary venue where rival readings contest ground.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Would argue that rights-bearing status should track demonstrable cognitive capacity rather than developmental potential, but this argument is foreclosed from legislative consideration in jurisdictions that have adopted the conception standard as binding law; their framework is treated as a fringe philosophical position rather than a live legal option.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, functional_capacity_theorists, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, bright-line rule for when legal personhood attaches, avoiding case-by-case adjudication of moral status and giving prosecutors, clinicians, and courts a determinate standard to apply.
% TRANSFER_FUNCTION: Moves decisional authority over pregnancy outcomes from the pregnant person and treating clinician to the state and to advocacy-shaped statute; moves liability exposure onto pregnant persons and providers; moves reputational and political capital to advocacy organizations and allied institutions.
% ABSENT_VOICES: Pregnant persons whose specific medical circumstances (ectopic pregnancy, fatal fetal anomaly, life-threatening complications) do not fit the bright-line rule are not consulted in the rule's design; functional-capacity theorists and comparative-fetal-development researchers are excluded from the legislative record that adopts the standard as settled.
% DISAPPEARANCE_RATIONALE: If the conception-based standard were withdrawn overnight, criminal exposure for pregnancy outcomes would collapse, IVF practice would revert to prior liability norms, prosecutorial authority over miscarriage and stillbirth would vanish, and reproductive healthcare practice would realign with pre-standard clinical judgment — a substantial rearrangement of both medical practice and criminal law.
% FOUNDING_PROBLEM: The kernel problem — when does an entity acquire rights-bearing moral and legal status — has no settled empirical answer; this reading was built to resolve the resulting indeterminacy by anchoring personhood to a discrete, verifiable biological event (conception) rather than to a continuous or contested developmental threshold.
% FOUNDING_PROBLEM_CORROBORATION: Fetal rights organizations and allied religious institutions attest the founding problem (moral status indeterminacy) is permanently live and best resolved by a bright-line rule. Bioethicists, maternal-fetal medicine associations, and constitutional scholars outside the advocacy coalition attest that the underlying indeterminacy is real but that a conception bright-line does not resolve it — it merely assigns the disputed answer legal force, and clinical outcomes data (from jurisdictions that adopted the standard) is cited by these outside sources as evidence the rule generates predicate harms it did not anticipate (delayed miscarriage care, IVF chilling effects).
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 and rising over the interval: as the standard is adopted and tested in more jurisdictions, its practical reach expands from headline abortion restriction into IVF liability, pregnancy-loss investigation, and substance-use-in-pregnancy prosecution — a widening extraction footprint consistent with the accumulation pattern. Suppression is authored higher still (0.79) because the standard's persistence depends on active enforcement machinery (criminal referral, civil liability exposure, licensing consequences for providers) rather than voluntary compliance; alternatives (traveling out of jurisdiction, informal harm-reduction practice) are actively targeted for closure, not merely disfavored. Theater ratio is kept low (0.22) because the enforcement activity is substantively consequential, not merely symbolic — prosecutions and liability judgments are real, not performative. Accessibility collapse (0.58) and resistance (0.81) reflect that alternative legal framings remain vigorously contested in courts and legislatures — this is not a settled mountain, it is an actively fought constructed boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons and reproductive healthcare providers are structural targets: the standard imposes costs (liability, loss of clinical discretion, criminal exposure) directly on them, with limited exit (relocating jurisdictions is costly and often unavailable). Fetal rights advocacy organizations and allied religious institutions are structural beneficiaries: they achieve legal vindication of a contested doctrine without bearing pregnancy-specific costs. State prosecutorial authorities are administrators who gain expanded jurisdiction — a genuine institutional beneficiary role distinct from the advocacy beneficiaries. IVF patients are a distinguishable victim class because the reading's logic (rights attach at conception) was designed with gestation in view but generates unanticipated liability in a wholly different clinical context (embryo storage/disposal), which is itself evidence the underlying premise is being extended beyond its originating rationale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — moral status indeterminacy — is genuinely unresolved and will likely remain so; this is not a mandatrophy case in the classic 'solved problem, persisting arrangement' sense. What IS contested is whether the SPECIFIC bright-line answer (conception) still serves the coordination function it was built for, given that its downstream consequences (IVF chilling effects, delayed miscarriage care) were not part of the original justification and are attested by outside clinical and bioethical sources as harms the rule does not anticipate or correct for. The tangled_rope classification captures this: there IS a genuine coordination function (a determinate standard reduces case-by-case adjudication cost) but it operates through the same structure that imposes asymmetric costs on pregnant persons and providers — coordination and extraction are bound together, not separable without dismantling the bright-line rule itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conception_as_natural_kind_or_constructed_line,
    'Is conception a natural kind boundary that personhood tracks, or a constructed line selected because it is biologically discrete and therefore administratively convenient, independent of its correspondence to moral status?',
    'No empirical resolution mechanism exists for the underlying moral-status question; what can be examined empirically is whether the doctrine''s downstream legal consequences (IVF liability, miscarriage prosecution) were anticipated by its original proponents or are treated as acceptable costs after the fact — divergence between original justification and current defense would support the constructed-line reading.',
    'If conception is best understood as an administratively convenient discrete marker rather than a principled moral-status threshold, the coordination function claimed for the standard is weaker than claimed, and the classification shifts toward snare (coordination story as cover for a doctrinal victory with asymmetric costs) rather than tangled_rope (genuine coordination coexisting with extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conception_as_natural_kind_or_constructed_line, conceptual, 'Whether the conception threshold is a natural-kind boundary or a constructed administrative convenience.').

omega_variable(
    kernel_reading_forecloses_restrictive_reading,
    'Does adopting the developmental-potentiality reading as binding law in a jurisdiction logically foreclose the restrictive anthropocentric reading in that same jurisdiction, or can both readings persist as unresolved doctrinal tensions within one legal system (e.g., differing state vs. federal treatment)?',
    'Comparative jurisdictional analysis: examine whether any single legal system has attempted to hold both a conception-based fetal personhood standard and a birth-based personhood standard simultaneously without one displacing the other in practice.',
    'If jurisdictions show durable coexistence (e.g., through federalism, where states diverge), the forecloses relation should be softened to influences at the national level even though it holds at the single-jurisdiction level; this affects how the cs_structure reading_relations should be interpreted across scales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_forecloses_restrictive_reading, conceptual, 'Whether foreclosure of the sibling reading holds only within a single jurisdiction or across an entire federated legal system.').

omega_variable(
    downstream_extraction_intent_vs_effect,
    'Were the IVF-liability and miscarriage-prosecution consequences of the conception standard intended by its drafters, or are they unintended structural spillovers of a rule designed with abortion restriction as its primary target?',
    'Legislative history analysis: examine floor debates, model statute drafting records, and advocacy organization public statements for explicit discussion of IVF and miscarriage-prosecution consequences at the time of adoption versus post-hoc defense of those consequences once litigated.',
    'If unintended, the extraction extending to IVF patients is better modeled as an emergent property of the bright-line logic rather than deliberate design, which would matter for how culpability and reform pressure are allocated among the beneficiary stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_extraction_intent_vs_effect, empirical, 'Whether spillover harms to IVF patients were designed or are unintended consequences of the bright-line rule''s internal logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lega_tr_t4, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(lega_tr_t12, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lega_be_t4, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(lega_be_t12, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lega_su_t4, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(lega_su_t12, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 24, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__developmental_potentiality_reading, 0.1).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, functional_capacity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'personhood boundary' question. Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct classification: developmental_potentiality_reading (this story, tangled_rope) forecloses restrictive_anthropocentric_reading within any single jurisdiction's binding law, while coexisting with functional_capacity_reading as a live but distinct philosophical/legal position in ongoing public and academic discourse. Network edges link all three; contamination or reform pressure on one reading's legitimacy propagates asymmetrically to the others depending on the reading_relations declared in each story's cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__developmental_potentiality_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
