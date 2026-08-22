% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Mandates as Graduated Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   State statutes condition entry into a widening set of occupations on
 *   completing prescribed education, passing examinations, paying fees, and
 *   maintaining renewal requirements, with boards drawn largely from
 *   incumbent licensees empowered to prosecute unlicensed practice. Read as a
 *   single colloquial label, 'occupational licensing' conflates structurally
 *   distinct claims; per the epsilon-invariance principle this file
 *   decomposes one reading — the graduated_access_filter reading, under which
 *   the operative function of the credential mandate is tiering market access
 *   by class and prior resource endowment, with barrier height tracking
 *   acquisition cost more tightly than occupational risk. The epsilon
 *   referent is the standing credential-mandate arrangement as this reading
 *   assesses it, never the deregulated alternative this reading would prefer.
 *   Assumptions: the story generalizes United States-style occupational
 *   licensing; the interval maps approximately onto 1950-2020 (T0 ~ 1950, T70
 *   ~ 2020), the period of licensing expansion from roughly five percent to
 *   roughly a quarter of the workforce. Claim and metrics are independent
 *   authored facts: the claimed type states what this reading holds
 *   structurally true; the metrics state what it holds descriptively true;
 *   the engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - credentialed_incumbents: primary beneficiary (organized/identity_locked) — collects the restricted-competition wage premium; professional identity fused with the credential
 *   - state_licensing_boards: agenda setter (institutional/arbitrage) — administers, fees, and enforces the mandate; dominated by incumbents
 *   - accredited_training_providers: secondary beneficiary (organized/mobile) — sells the mandated preparation hours
 *   - low_resource_aspiring_workers: primary target (powerless/constrained) — bears the barrier's time and money costs before earning anything
 *   - unlicensed_practitioners_facing_enforcement: target under active enforcement (powerless/trapped) — practices informally under legal threat
 *   - consumers_of_licensed_services: dual-positioned payer/beneficiary (organized/mobile) — pays supply-restricted prices, receives a uniform status signal
 *   - immigrants_with_foreign_credentials: excluded voice (powerless/trapped) — qualifications unrecognized, no seat in the conversation
 *   - reform_coalitions_and_litigators: analytical observer (organized/analytical) — litigates and studies the whole structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.76).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.72).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.76).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Mandates as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '1ace1daf-11b4-4a96-a351-aa29b6da7ee7').
narrative_ontology:cs_kernel_codification('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', fixed_text).
narrative_ontology:cs_authority_grounding('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', extraction).
narrative_ontology:cs_interpretation_layer_present('1ace1daf-11b4-4a96-a351-aa29b6da7ee7').
narrative_ontology:cs_reading_relation('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', licensing_statute_mandate__rent_seeking_suppression, influences).
narrative_ontology:cs_axiom('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', foundational, market_access_must_not_track_prior_resources).
narrative_ontology:cs_axiom_status(market_access_must_not_track_prior_resources, holdable).
narrative_ontology:cs_axiom_grounding('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', market_access_must_not_track_prior_resources, deontological).
narrative_ontology:cs_axiom('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', secondary, barrier_height_must_track_occupational_risk).
narrative_ontology:cs_axiom_status(barrier_height_must_track_occupational_risk, holdable).
narrative_ontology:cs_axiom_grounding('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', barrier_height_must_track_occupational_risk, instrumental).
narrative_ontology:cs_reference_frame('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', risk_proportionate_access_baseline).
narrative_ontology:cs_drift_state('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', contemporary_mass_licensing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1ace1daf-11b4-4a96-a351-aa29b6da7ee7', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbents).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, state_licensing_boards).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, accredited_training_providers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, low_resource_aspiring_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, unlicensed_practitioners_facing_enforcement).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, class_reproduction_via_gatekeeping).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold licenses in the covered occupations and earn a measurable wage premium from reduced competition. Their training investment and professional standing are bound up with the credential, and many serve on the boards that set entry rules. Leaving the occupation would forfeit the sunk cost of qualification, so defending the requirement is also defending their own past expenditure and self-concept as a credentialed professional.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_incumbents, beneficiary,
    organized, generational, identity_locked, national).

% Agency bodies, typically composed predominantly of incumbent licensees, that write implementing rules, set fees, approve training programs, and prosecute unlawful practice. They are funded largely by licensee fees. When budgets tighten or scrutiny rises they can raise fees or expand the list of covered activities rather than absorb costs; abolition would require legislative action they help shape through testimony and drafting input.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, state_licensing_boards, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, state_licensing_boards, beneficiary).

% Schools and course vendors selling the mandated classroom hours and exam preparation. Enrollment is guaranteed by the requirement itself regardless of job-market conditions; when one occupation's rules tighten they shift curricula and marketing toward newly licensed fields.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, accredited_training_providers, beneficiary,
    organized, generational, mobile, national).

% Would-be entrants who face tuition, exam fees, unpaid supervised hours, and travel to testing sites before earning anything in the occupation. Those without savings, credit, or family support either delay entry for years, take on debt, or choose unregulated work that pays less. Switching to another occupation is possible but usually at a lasting pay cut, so exit exists but is expensive.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, low_resource_aspiring_workers, payer,
    powerless, biographical, constrained, national).

% People already practicing the trade informally — hair braiders, interior decorators, teeth-whitening technicians and similar services — who operate outside the legal channel. Cease-and-desist letters, fines, and in some jurisdictions criminal charges hang over continued work; coming into compliance means completing the full hour and fee requirements mid-career, which most cannot fund.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, unlicensed_practitioners_facing_enforcement, payer,
    powerless, immediate, trapped, regional).

% Buy fewer and pricier services than an open market would offer, especially in low-risk categories where supply is thinnest. They also receive a uniform, verifiable signal of practitioner status and a complaint channel. They can substitute do-it-yourself work, delay purchases, or hire informally, so their exposure is real but partially avoidable.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, beneficiary).

% Trained abroad as nurses, electricians, or cosmetologists, whose existing qualifications are not recognized and who often learn of retraining demands only after arrival. They hold no seat on boards and rarely participate in rulemaking comment periods; the practical option available to most is leaving the occupation entirely.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, immigrants_with_foreign_credentials, excluded,
    powerless, biographical, trapped, global).

% Legal-defense organizations, labor economists, and bipartisan reform campaigns that bring economic-liberty challenges, publish stringency studies, and push universal-recognition and sunset-review legislation. They see the whole structure across occupations and can force remedies, but they hold no administrative seat in the system they study.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, reform_coalitions_and_litigators, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, credentialed_incumbents).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, state-backed threshold marking who may lawfully practice in designated occupations, giving consumers a common signal of practitioner status and practitioners a defined boundary around lawful work.
% TRANSFER_FUNCTION: Moves market access, and the income attached to it, from workers unable to meet the credential's time and money requirements toward those who already hold or can afford the credential; moves fee and tuition revenue from entrants to boards and training providers.
% ABSENT_VOICES: Rejected applicants, immigrants with unrecognized foreign training, and consumers paying supply-restricted prices have no seat: boards are composed predominantly of incumbent licensees, and rulemaking notices reach trade associations far more reliably than affected outsiders. Their objections surface mainly in litigation filings and occasional sunset hearings.
% DISAPPEARANCE_RATIONALE: Wages in licensed occupations would compress toward open-market levels as supply expanded; large numbers of informal practitioners would legalize overnight; the training industry built on mandated hours would shrink sharply; boards would lose their fee base and most would dissolve; prices for affected services would fall while quality assurance migrated to certification schemes, bonding, insurance, and reputation systems.
% FOUNDING_PROBLEM: Protecting clients and the public from demonstrably dangerous incompetence in a small set of high-stakes trades — medicine, law, engineering — where a single mistake maims or kills.
% FOUNDING_PROBLEM_CORROBORATION: Public-health and malpractice literature corroborates that the founding problem is live for the narrow high-risk core. Occupational-licensing scholarship from outside the benefiting parties attests that the problem does not extend to most of the hundreds of occupations now covered. No source outside the benefiting parties attests that the founding problem justifies the current scope; the broadened-scope justification originates almost entirely with boards and incumbent associations.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76 at interval end) because the barrier's cost falls hardest on those with the fewest resources while its wage premium accrues to those already inside; the transfer is regressive by construction. Suppression (0.72) is a raw structural property, deliberately unscaled: boards actively prosecute unlicensed practice, resist certification substitutes, and fight interstate portability, so persistence depends on coercive maintenance rather than participant preference. Theater ratio (0.40) reflects a growing performative share — continuing-education mandates, renewal rituals, and board process that maintain the appearance of vigilance — without approaching piton-level decay, because enforcement still bites. Accessibility collapse (0.62) is substantial but incomplete: working legally in the occupation without the credential collapses once the mandate is understood, yet partial exits remain (informal practice, adjacent unregulated trades, a few state alternate-certification paths). Resistance (0.58) is real and rising: economic-liberty litigation, universal-recognition campaigns, and sunset-review movements. The measurement series run on one shared eight-point grid — every tracked metric is authored at every examined time point — modeling an enforcement ratchet: suppression_requirement rises as board budgets, prosecution capacity, and continuing-education mandates matured over the interval, which is why the enforcement-capacity series is authored rather than left to the static scalar. Coalition check: the powerless victim seats have latent coalition potential (rank-and-file practitioner associations, platform-economy alliances), but the barrier itself selects against organizers — it filters precisely on the resources that fund collective action — which is why measured resistance stays moderate despite diffuse grievance. Scope note: the mandate operates at national scale across fifty state variants, and larger scope raises verification difficulty, which the engine applies as an amplification on effective extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the board and incumbent positions the arrangement is a maintained professional standard they staff, fund, and personally embody; from the aspiring-worker and unlicensed-practitioner positions the same structure is a priced gate that sorts by parental resources; from the consumer position it is both a price markup and a usable trust signal. Same-level divergence is equally structural: credentialed incumbents and low-resource aspirants hold comparable nominal standing as labor-market participants, but the credential converts identical ambition into opposite directionalities — the incumbent's sunk qualification buys identity-locked defense of the gate, while the aspirant's lack of it buys constrained exit. The engine computes these divergences from the power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for credentialed_incumbents, accredited_training_providers, and state_licensing_boards (the last doubly so as agenda setter collecting fees). Victim declarations drive high directionality for low_resource_aspiring_workers and unlicensed_practitioners_facing_enforcement, amplified by constrained and trapped exit respectively — trapped or immobile targets sit nearer the full-target end. Consumers_of_licensed_services derive near-symmetric directionality from their dual payer/beneficiary position, tilted slightly toward target by the supply-restricted price. Immigrants_with_foreign_credentials are excluded rather than coordinated: their exclusion is part of what the enforcement machinery maintains. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate every seat, so the structural derivation chain suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — guarding against dangerous incompetence in high-stakes trades — is genuinely live for a narrow core and dead or irrelevant for much of the current coverage, hence founding_problem_status is contested rather than dead: declaring it resolved would overclaim, and the status-by-verdict pair (contested x world_rearranges) correctly produces no zombie flag while still exposing the drift whereby the arrangement widened as its original justification narrowed. Classification discipline cuts both ways. Reading the whole arrangement as pure coordination erases the asymmetric burden this reading documents; reading it as pure extraction with no residual function erases the real screening that persists at the margin in high-risk occupations. The snare claim keeps both facts visible: a genuine coordination shell with extraction as the operative function and identifiable victims. Piton is rejected because the function has not atrophied into performance — enforcement still changes behavior — and rope is rejected because the burden distribution is asymmetric and alternatives are actively suppressed rather than merely unavailable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the graduated_access_filter reading of the licensing_statute_mandate kernel; what would the sibling readings (public_safety_coordination, rent_seeking_suppression) change structurally, and where exactly does the disagreement sit?',
    'Cross-reading comparison of the three family stories: align each reading''s epsilon, beneficiary/victim sets, and computed types. The disagreement locates in the operative function attributed to the credential barrier — competence floor versus class-sorted filter versus supply-restricting cartel device.',
    'If the public_safety reading prevails for an occupation subset, that subset decomposes into a separate lower-epsilon constraint with a different victim set; if the rent_seeking reading prevails, the beneficiary set narrows to organized incumbents and the class-sorting mechanism becomes downstream consequence rather than defining structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of the licensing_statute_mandate kernel, with the disagreement located in the barrier''s operative function.').

omega_variable(
    barrier_stringency_risk_correlation,
    'Does barrier stringency (required hours, fees, examination difficulty) track occupation-specific risk of harm, or does it track incumbent political organization and the resources of the licensed class?',
    'Cross-occupation regression of licensing stringency on injury and error rates versus measures of incumbent organization (board composition, association density), controlling for training externalities.',
    'If stringency tracks risk, a component of measured extraction is genuine coordination cost and effective extraction falls toward hybrid-coordination territory; if it tracks organization, the exclusion mechanism is confirmed as the primary function and the classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_stringency_risk_correlation, empirical, 'Whether the credential barrier''s height encodes occupational risk or class and political resources.').

omega_variable(
    internalized_worthiness_deficit,
    'Is the observed non-entry of low-resource workers into licensed occupations purely price rationing, or does it partly reflect an internalized deficit in which aspirants conclude they do not belong before ever pricing the barrier?',
    'Post-reform natural experiments (fee waivers, universal recognition, apprenticeship alternates): if application and completion rates rise faster than price effects alone predict, part of the filtering was internalized; persistent participation gaps after cost removal indicate suppression carried by the targets themselves.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the barrier with them after formal removal, and remediation requires more than statutory change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_worthiness_deficit, empirical, 'Structural versus internalized mechanism behind class-sorted non-entry.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel best framed as the statutory text (fixed, legislatively amended) or as the boards'' accumulated administrative practice (implicit, whatever boards enforce), and does the choice change the commitment-system classification?',
    'Classify under both framings and compare: statute-as-kernel with a board interpretation layer versus practice-as-kernel where the kernel is whatever boards currently enforce. Signals guiding the present choice: amendments require legislation while scope creep happens administratively, suggesting the operative kernel lives partly below the text.',
    'Under the practice-as-kernel framing, authority grounding shifts toward practice and codification toward implicit, weakening the fixed-text stability claim and raising measured drift; under the statute framing, the present declaration stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Commitment-system framing under-determination between statutory text and administrative practice as the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.18).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.22).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.26).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.3).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.33).
narrative_ontology:measurement(lice_tr_t50, licensing_statute_mandate__graduated_access_filter, theater_ratio, 50, 0.36).
narrative_ontology:measurement(lice_tr_t60, licensing_statute_mandate__graduated_access_filter, theater_ratio, 60, 0.38).
narrative_ontology:measurement(lice_tr_t70, licensing_statute_mandate__graduated_access_filter, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(lice_be_t50, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(lice_be_t60, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 60, 0.73).
narrative_ontology:measurement(lice_be_t70, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 70, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(lice_su_t50, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 50, 0.67).
narrative_ontology:measurement(lice_su_t60, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(lice_su_t70, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, identity_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'occupational licensing' into three epsilon-invariant readings of one kernel, per the epsilon-invariance principle: public_safety_coordination (low extraction, coordination-side), rent_seeking_suppression (high extraction, incumbent-centered), and graduated_access_filter (this file; high extraction, entrant/class-centered). Each story carries its own epsilon, beneficiary/victim structure, and claimed type; family linkage runs through affects_constraints. Direction of influence: the public_safety story is cited as legitimating evidence by the other two; this reading feeds the distributional substrate (who bears the barrier) that the rent_seeking story requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
