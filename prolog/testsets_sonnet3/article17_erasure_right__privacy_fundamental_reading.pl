% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: GDPR Article 17 Right to Erasure — Data Sovereignty Reading
 *   domain: technology_governance/data_protection_law
 *
 * SUMMARY:
 *   This story instantiates the privacy_fundamental_reading of the Article 17
 *   (GDPR right to erasure) kernel: individual data sovereignty as a
 *   fundamental right that limits corporate data retention. Under this
 *   reading, the primary structural relationship is a genuine coordination
 *   fix for a real asymmetry — personal data outliving the relationship or
 *   consent that generated it, with no lever available to the individual
 *   whose data it is. The right is broad in scope, low in epistemic friction
 *   (requests are not gated by proving harm), and enforced by data protection
 *   authorities against controllers who bear the compliance cost. This is a
 *   distinct constraint from the sibling readings of the same kernel text —
 *   the competitive_moat_reading (where compliance cost asymmetry favors
 *   incumbents) and the censorship_mechanism_reading (where erasure requests
 *   are weaponized against speech) — which are authored as separate
 *   constraint files per the ε-invariance principle, since they carry
 *   different ε, different beneficiaries, and different victim sets from the
 *   same underlying provision.
 *
 * KEY AGENTS:
 *   - data_subjects: primary beneficiary (powerless/constrained) — gains an enforceable deletion lever
 *   - individual_platform_users: primary beneficiary (powerless/constrained) — gains exit from persistent data exposure
 *   - data_controllers: constrained payer (institutional/constrained) — bears compliance infrastructure cost
 *   - data_protection_authorities: agenda_setter (institutional/analytical) — interprets scope and adjudicates
 *   - downstream_processors: secondary payer (moderate/constrained) — extended compliance chain
 *   - public_interest_researchers: excluded — bears downstream effects without a seat in individual decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.18).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.22).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "GDPR Article 17 Right to Erasure — Data Sovereignty Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/data_protection_law").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '3eb11023-1071-427e-abd8-b15ea1938d76').
narrative_ontology:cs_kernel_codification('3eb11023-1071-427e-abd8-b15ea1938d76', fixed_text).
narrative_ontology:cs_authority_grounding('3eb11023-1071-427e-abd8-b15ea1938d76', lineage).
narrative_ontology:cs_interpretation_layer_present('3eb11023-1071-427e-abd8-b15ea1938d76').
narrative_ontology:cs_reading_relation('3eb11023-1071-427e-abd8-b15ea1938d76', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('3eb11023-1071-427e-abd8-b15ea1938d76', article17_erasure_right__censorship_mechanism_reading, influences).
narrative_ontology:cs_axiom('3eb11023-1071-427e-abd8-b15ea1938d76', foundational, individual_informational_self_determination_is_fundamental).
narrative_ontology:cs_axiom_status(individual_informational_self_determination_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('3eb11023-1071-427e-abd8-b15ea1938d76', individual_informational_self_determination_is_fundamental, deontological).
narrative_ontology:cs_axiom('3eb11023-1071-427e-abd8-b15ea1938d76', secondary, retention_absent_ongoing_purpose_is_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(retention_absent_ongoing_purpose_is_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('3eb11023-1071-427e-abd8-b15ea1938d76', retention_absent_ongoing_purpose_is_presumptively_illegitimate, conventional).
narrative_ontology:cs_reference_frame('3eb11023-1071-427e-abd8-b15ea1938d76', informational_self_determination_baseline).
narrative_ontology:cs_drift_state('3eb11023-1071-427e-abd8-b15ea1938d76', post_google_spain_enforcement_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3eb11023-1071-427e-abd8-b15ea1938d76', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, individual_platform_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, downstream_processors).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, informational_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% An individual whose personal data sits on a corporate server can file an erasure request under Article 17, requiring deletion absent an overriding legal basis. Before this right existed, the person had no structural lever over data that outlived the relationship that generated it — an old account, a stale photo, a rescinded consent. The request costs little, is not gated by proving harm, and applies broadly across sectors and platforms.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects, beneficiary,
    powerless, biographical, constrained, national).

% A user closing an account or withdrawing from a service can compel deletion of the data trail that would otherwise persist indefinitely for profiling, resale, or breach exposure. From this seat the right converts an asymmetric relationship (the platform decides what happens to the data) into one where the individual has an enforceable exit from the data relationship, independent of the underlying service relationship.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, individual_platform_users, beneficiary,
    powerless, biographical, constrained, national).

% A company holding personal data must build request-intake, verification, and deletion-propagation infrastructure (including to downstream processors and cached/backup copies), and must justify any refusal against narrow statutory exemptions. From this seat, individually meritorious requests are aggregated into an ongoing compliance cost, but the obligation is bounded by defined exemptions (legal obligation, public interest, freedom of expression, defense of legal claims) rather than open-ended.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_controllers, payer,
    institutional, generational, constrained, continental).

% National and EU-level regulators interpret the scope of erasure obligations, adjudicate complaints, and set enforcement priorities. They administer the balance between the fundamental-rights reading and the exemptions, and their guidance materially shapes how broadly or narrowly 'erasure' is read in practice.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Third parties who received data from a controller (ad networks, analytics vendors, sub-processors) must also comply once notified, extending the compliance chain beyond the original relationship. Their exit options are bounded by contractual dependency on the controllers who route them data.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, downstream_processors, payer,
    moderate, biographical, constrained, continental).

% Researchers relying on longitudinal personal-data datasets are not party to individual erasure decisions but bear downstream effects when records they depend on are deleted; their objection (that broad erasure degrades public-interest research value) is raised in policy debate but is not a decision input to any single erasure request.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, public_interest_researchers, excluded,
    moderate, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, low-friction mechanism by which any individual can compel deletion of their personal data from a controller's systems, replacing a patchwork of unenforceable expectations with a single enforceable right — solving the genuine coordination problem of data outliving the relationship or consent that generated it.
% TRANSFER_FUNCTION: Moves control over the disposition of personal data from the entity holding it (which previously could retain, monetize, or expose it indefinitely) to the individual it describes, at the cost of compliance infrastructure borne by controllers and downstream processors.
% ABSENT_VOICES: Public-interest researchers and archival institutions are not parties to individual erasure decisions and are not represented in the request-by-request adjudication, even though aggregate erasure activity can degrade datasets they depend on; this concern surfaces in policy review, not in the mechanism itself.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished, individuals would have no enforceable mechanism to compel deletion of data outliving its original purpose; platforms could retain indefinitely absent voluntary policy, and the current baseline of routine deletion-on-request across the EU digital economy would collapse back to unilateral controller discretion.
% FOUNDING_PROBLEM: Personal data generated in one context (a purchased service, a discarded account, a withdrawn consent) persisted indefinitely on corporate servers with no individual mechanism to compel its removal, creating growing exposure to profiling, resale, and breach risk with no correlate to the individual's actual ongoing relationship with the data holder.
% FOUNDING_PROBLEM_CORROBORATION: Independent surveys by consumer-protection bodies and academic privacy researchers (outside both the regulated industry and the individual claimants) document continuing high volumes of erasure requests tied to closed accounts, data breaches, and stale profiling data, corroborating that the underlying retention problem the right addresses remains active rather than historical.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).
:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18-0.22) because, under this reading, the right transfers control TO the powerless party rather than extracting from them — the coordination function (giving individuals a lever over persistent data) dominates. Suppression is moderate-low (0.22): the right does constrain controller conduct through legal force, but this is asymmetric constraint applied to protect a party, not coercion applied against one. Theater ratio starts higher (0.28) reflecting early-implementation performative compliance (privacy notices, checkbox consent theater around the broader GDPR regime) and falls over the interval (0.15) as genuine deletion pipelines matured and enforcement precedent solidified the substantive interpretation. Accessibility collapse (0.35) and resistance (0.4) are both moderate, reflecting that this is a constructed legal right, not a natural law — real institutional resistance from data controllers persists, and exemptions (legal obligation, freedom of expression, public interest) leave meaningful carve-outs rather than complete collapse of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the data_subject seat, this reads straightforwardly as a Rope: a coordination fix that resolves a genuine asymmetry with the individual as net beneficiary and no coercive overhead falling on them. From the data_controller seat, the same structure is experienced as an enforceable external constraint with real compliance cost — but that experienced cost does not, under this reading, indicate victimhood; it indicates the ordinary operation of a right that successfully constrains the previously-unconstrained party. The engine should compute these as different experienced burdens without collapsing the classification into tangled_rope, because no identifiable victim group bears asymmetric extraction under this reading — the sibling readings (moat, censorship) are where victims appear.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects and individual users are declared beneficiaries with powerless structural position but the right itself inverts the default power asymmetry — the engine's directionality derivation should place them near the beneficiary end (low d) because the constraint subsidizes their interests despite their low nominal power, which is the whole point of the right. Data controllers and downstream processors are payers who bear compliance costs; their institutional power does not translate into structural benefit from this specific mechanism — they are the constrained party under this reading. This is a case where nominal power (institutional) and directional relationship to the constraint (target/payer) diverge, which is exactly the structural inversion a genuine individual-protective right should produce.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncontrolled indefinite retention of personal data with no individual recourse) remains live per corroborating survey data from consumer-protection and academic sources outside both industry and claimant populations — this is not a mandate that has outlived its function. Classifying this reading as Rope rather than Tangled Rope or Snare prevents mislabeling a successful protective coordination mechanism as extraction merely because it imposes real, enforceable costs on powerful institutional actors; cost-bearing by the constrained party is not itself evidence of victimhood when the mechanism's purpose is precisely to constrain that party on behalf of a genuinely disadvantaged one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    erasure_scope_boundary_ambiguity,
    'Does the ''broad erasure interpretation'' this reading assumes hold consistently across DPA guidance and CJEU jurisprudence, or is the scope narrower and more exemption-laden in practice than the fundamental-rights framing suggests?',
    'Systematic review of DPA enforcement decisions and CJEU rulings (e.g., Google Spain progeny) tracking how often erasure requests are granted in full versus narrowed by exemption, over a multi-year window.',
    'If in-practice scope is substantially narrower than ''broad interpretation, low friction'' implies, this reading''s own ε may be understated relative to the friction actually experienced by data subjects, and the reading''s structural claim (low epistemic friction) would need revision independent of the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erasure_scope_boundary_ambiguity, empirical, 'Whether broad-interpretation, low-friction erasure holds in practice or is narrower than the reading assumes.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the single-text Article 17 provision better modeled as one kernel with three readings (as done here), or does the diversity of enforcement contexts (individual consumer requests vs. platform-scale takedown mechanisms vs. cross-border erasure-of-search-results cases) actually constitute distinct legal instruments that only nominally share a text?',
    'Comparative doctrinal analysis of how courts distinguish ''core'' erasure (Article 17(1)) from search-delisting erasure (Google Spain line) from platform content-moderation erasure requests, to determine whether these are genuinely one contested kernel or three instruments wearing one label.',
    'If they are genuinely distinct instruments, the reading_relations declared here (coexists_with the moat and censorship readings) understate the separation — they would not be readings of one kernel but effectively unrelated provisions, changing how contamination/network analysis should treat them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three declared readings share a genuine single kernel or are better modeled as separate instruments.').

omega_variable(
    beneficiary_vs_constructed_right_ambiguity,
    'Is data sovereignty a natural extension of pre-existing personhood/autonomy interests being formally recognized (making the right closer to a discovered protection), or is it a fully constructed legal entitlement whose boundaries are contingent on regulatory politics and could have been drawn very differently?',
    'Comparative legal history across jurisdictions with and without a comparable erasure right, examining whether autonomy-interest arguments predate and are independent of the specific GDPR text, versus being retrofitted justifications for a politically negotiated compromise.',
    'If substantially constructed rather than discovered, the fundamental-rights framing itself (not just the sibling readings) carries some of the contingency the moat/censorship readings foreground, which would soften the sharp separation between this reading and its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_vs_constructed_right_ambiguity, conceptual, 'Whether the fundamental-rights framing itself rests on discovered or constructed grounds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(arti_tr_t8, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(arti_tr_t16, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(arti_tr_t20, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(arti_tr_t24, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 12, 0.19).
narrative_ontology:measurement(arti_be_t16, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement(arti_be_t20, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(arti_be_t24, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 24, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article17_erasure_right__privacy_fundamental_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposed from the single natural-language label 'Article 17 right to erasure,' per the ε-invariance principle. privacy_fundamental_reading (this file) authors low ε (~0.18-0.22) with data subjects as beneficiaries and no victims — a Rope. competitive_moat_reading authors higher ε with platforms as beneficiaries and smaller competitors/new entrants as victims — expected Tangled Rope or Snare depending on enforcement asymmetry. censorship_mechanism_reading authors ε reflecting erasure-as-speech-suppression, with speakers, journalists, and archives as victims — expected Tangled Rope or Snare. All three share the same kernel text but diverge sharply in beneficiary/victim structure and ε, which is why they are three files linked by network edges rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
