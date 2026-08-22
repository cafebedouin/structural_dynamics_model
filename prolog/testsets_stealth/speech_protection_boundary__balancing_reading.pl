% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Case-by-Case Speech Protection Boundary (Balancing Reading)
 *   domain: constitutional law/political philosophy
 *
 * SUMMARY:
 *   Under this reading of the speech-protection kernel, whether expression is
 *   constitutionally protected is not fixed by category but determined
 *   case-by-case: a court weighs the First Amendment interest against
 *   competing constitutional values (equality, dignity, safety, privacy,
 *   national security) and against demonstrated harms, and the boundary
 *   between protected and unprotected speech shifts with context. The
 *   standing arrangement under contest — and the sole referent of epsilon
 *   here — is this weighing regime as actually operated by the courts: its
 *   multi-factor tests, its intermediate-scrutiny layer for coded and
 *   systemic-harm claims, and its distribution of the gatekeeper role across
 *   the judiciary rather than into categorical rules. Sibling readings
 *   (absolutist, harm_limited) are separate constraints in separate files and
 *   are not averaged into this one. KEY AGENTS (by structural relationship):
 *   - appellate_judiciary: Agenda-setter and principal collector
 *   (institutional/constrained) — administers every weighing and accumulates
 *   the interpretive authority the method generates -
 *   harassment_dignity_claimants: Secondary beneficiary
 *   (moderate/constrained) — their demonstrated harm counts as a weight -
 *   value_conflicted_speakers: Dual-positioned beneficiary-payer
 *   (moderate/constrained) — individualized hearing gained, unpredictability
 *   cost borne - unpopular_minority_speakers: Primary target
 *   (powerless/identity_locked) — weighed against majority values they rarely
 *   beat, unable to exit their own expression - ordinary_litigants: Payer
 *   (moderate/constrained) — bears the cost of doctrinal indeterminacy case
 *   after case - affected_nonparty_communities: Excluded voice
 *   (powerless/trapped) — interests processed as abstractions, never seated -
 *   first_amendment_scholars: Analytical observer — maps the gap between
 *   announced method and observed outcomes
 *
 * KEY AGENTS:
 *   - appellate_judiciary: agenda-setter and principal collector (institutional/constrained) — administers the weighing, captures the gatekeeping authority it generates
 *   - harassment_dignity_claimants: secondary beneficiary (moderate/constrained) — demonstrated harm enters the weighing
 *   - value_conflicted_speakers: dual-positioned beneficiary-payer (moderate/constrained) — individualized hearing versus outcome uncertainty
 *   - unpopular_minority_speakers: primary target (powerless/identity_locked) — expression fused with identity, scales tilted toward majority values
 *   - ordinary_litigants: payer (moderate/constrained) — bears indeterminacy costs on every dispute
 *   - affected_nonparty_communities: excluded voice (powerless/trapped) — interests invoked as abstractions, no procedural seat
 *   - first_amendment_scholars: analytical observer (analytical/analytical) — documents method-outcome divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.48).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.42).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Case-by-Case Speech Protection Boundary (Balancing Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional law/political philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '9a232e12-3bd2-460f-8f14-74a32df4d9b6').
narrative_ontology:cs_kernel_codification('9a232e12-3bd2-460f-8f14-74a32df4d9b6', fixed_text).
narrative_ontology:cs_authority_grounding('9a232e12-3bd2-460f-8f14-74a32df4d9b6', lineage).
narrative_ontology:cs_interpretation_layer_present('9a232e12-3bd2-460f-8f14-74a32df4d9b6').
narrative_ontology:cs_reading_relation('9a232e12-3bd2-460f-8f14-74a32df4d9b6', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a232e12-3bd2-460f-8f14-74a32df4d9b6', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('9a232e12-3bd2-460f-8f14-74a32df4d9b6', foundational, protection_requires_contextual_weighing).
narrative_ontology:cs_axiom_status(protection_requires_contextual_weighing, holdable).
narrative_ontology:cs_axiom_grounding('9a232e12-3bd2-460f-8f14-74a32df4d9b6', protection_requires_contextual_weighing, instrumental).
narrative_ontology:cs_axiom('9a232e12-3bd2-460f-8f14-74a32df4d9b6', secondary, systemic_harm_receives_intermediate_scrutiny).
narrative_ontology:cs_axiom_status(systemic_harm_receives_intermediate_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('9a232e12-3bd2-460f-8f14-74a32df4d9b6', systemic_harm_receives_intermediate_scrutiny, empirically_contingent).
narrative_ontology:cs_reference_frame('9a232e12-3bd2-460f-8f14-74a32df4d9b6', first_amendment_as_contextual_mandate).
narrative_ontology:cs_drift_state('9a232e12-3bd2-460f-8f14-74a32df4d9b6', contemporary_categorical_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9a232e12-3bd2-460f-8f14-74a32df4d9b6', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, appellate_judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, harassment_dignity_claimants).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, value_conflicted_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, unpopular_minority_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, ordinary_litigants).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, affected_nonparty_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, value_conflicted_speakers).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, judicial_interstitial_rulemaking).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which speech counts as protected by weighing expressive interests against competing constitutional values and demonstrated harms, case by case. Each decision extends or limits the bench's own discretion, and the method keeps the hardest calls inside the courthouse rather than fixing them in advance. Tenured judges cannot decline the docket or hand the method to another institution; their way out of a bad weighing is only another weighing.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% People targeted by threatening, harassing, or dignity-stripping speech. Under this arrangement their demonstrated harm counts as a weight in the decision rather than losing automatically to a categorical speech-first rule. Their remedy arrives only through litigation they must fund themselves; losing means the conduct stays lawful and they bear the costs of having tried.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, harassment_dignity_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose expression mixes recognizable social value with potential for harm — security-related journalism, offensive satire, protest at charged sites. They receive an individualized hearing instead of automatic exclusion, but they cannot know in advance how the weighing will come out, and preparing for it consumes resources whether they win or lose.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, value_conflicted_speakers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, value_conflicted_speakers, payer).

% Speakers whose views carry little weight against majority-endorsed values — dissidents, fringe religious and political voices, unpopular protesters. When their cases are weighed, the scales rarely favor them, and the unpredictability of the method presses hardest on those least able to absorb an adverse ruling. Their expressive practice is typically fused with who they are: staying silent is not a real option, and leaving the jurisdiction rarely is either.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, unpopular_minority_speakers, payer,
    powerless, biographical, identity_locked, national).

% Parties to speech disputes who must litigate under a method whose outcome depends on which facts and values the deciding panel emphasizes. Every dispute becomes a full constitutional proceeding; settlement leverage and fee exposure both track the unpredictability. There is no cheaper, more predictable forum to opt into.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, ordinary_litigants, payer,
    moderate, immediate, constrained, national).

% Groups harmed by contested speech who are not parties to the cases that set the boundary. Their interests reach the court only as abstractions a judge chooses to invoke; they hold no procedural slot in the weighing. Their recourse is persuasion aimed at litigants they do not control, and they cannot exit the speech environment the decisions govern.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, affected_nonparty_communities, excluded,
    powerless, generational, trapped, national).

% Academic observers who code outcomes against the announced factors, document where results track panel composition rather than the stated method, and supply the competing readings that keep the interpretive contest alive. Nothing material flows to or from them under the arrangement; their seat is analytical.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, first_amendment_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, appellate_judiciary).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared adjudicative procedure for the recurring conflict between expressive liberty and competing constitutional commitments (equality, dignity, safety, privacy, national security) in cases where categorical rules are demonstrably over- or under-inclusive. It lets courts decide novel speech conflicts without a fixed statutory boundary, and gives legislatures a known (if demanding) test their speech regulations must survive.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed rules to sitting judges — each weighing allocates gatekeeping discretion anew; moves litigation costs and outcome-risk onto speakers and claimants; and allocates protection case-by-case between speakers and those harmed by speech.
% ABSENT_VOICES: Communities harmed by contested speech who never become litigants — their interests enter only as judge-invoked values, never as seated voices. Also resource-poor speakers, for whom the boundary is set entirely by cases others bring. Both classes are structurally outside the courtroom conversation that fixes the boundary.
% DISAPPEARANCE_RATIONALE: If case-by-case balancing vanished overnight, the doctrines that run through it — true threats, employee speech, commercial speech, campaign finance, harassment — would lose their decision procedure; hundreds of settled outcomes would need re-adjudication under some replacement method; the judiciary's gatekeeping role would contract sharply; and legislatures would face a different rule for drafting speech laws.
% FOUNDING_PROBLEM: Categorical speech rules proved unable to handle genuinely novel conflicts — threats, harassment, leaks, new media, election spending — where neither 'all speech wins' nor 'harmful speech loses' matched considered judgments. Courts needed a method for fitting protection to circumstance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: state legislatures continue enacting speech regulations that force case-by-case resolution, attesting the underlying conflicts persist; dissenting opinions by categorical-rule justices concede the hard cases are real while disputing the method; comparative-law scholarship documents the same conflict structure in other jurisdictions. The continuing docket itself is external evidence no beneficiary attestation could substitute for.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope, authored independently of the metrics: the regime possesses a genuine coordination function (hard-case resolution that categorical rivals demonstrably cannot replace — the founding problem is corroborated by the continuing docket) AND asymmetric extraction (interpretive authority accrues to the bench while uncertainty costs, chilling pressure, and unseated interests fall on speakers, litigants, and non-party communities), held together by active enforcement (every protection boundary exists only because a court performs the weighing). Metrics are authored descriptively. Extractiveness 0.48: substantial but bounded — the regime delivers real protective value to claimants and real flexibility to valuable-but-risky speech, so it is far from snare territory. Suppression 0.42, authored as a raw structural property and left unscaled: the regime coerces through unpredictability and litigation burden rather than direct force, and rival readings remain live in scholarship and dissents, so alternatives are only partly collapsed (accessibility_collapse 0.35). Resistance 0.55: an organized categorical-rule wing on the bench, a sustained originalist academy, and recurring legislative frustration keep the method continuously contested. Theater_ratio 0.28: the factor lists do real decisional work, but a visible fraction of the apparatus legitimates outcomes that track panel composition — theatrical maintenance without which the discretion would look bare. The temporal series run on one shared eight-point grid (all three metrics authored at every point, endpoints matching the scalar base_properties). The trajectories are rise-peak-decline, not monotonic: extraction, theater, and enforcement machinery all climbed through the great test-building era (multi-factor proliferation peaked discretion around T40), then receded modestly as categorical scrutiny revived for content-based restrictions and openly repudiated ad hoc balancing in several domains. Suppression_requirement is tracked because the story genuinely traces enforcement-capacity change — the machinery was built up and then partially dismantled — not merely extraction shifting.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the bench, the arrangement is craft: the only honest response to conflicts no rule anticipated, and each weighing is an exercise of responsibility the judges did not choose but cannot decline. From the claimant seat it is access — the first regime in which their harm counts. From the speaker seats it is roulette: identical conduct yields opposite outcomes across panels, and the identity-locked speaker cannot wait out a bad draw. From the excluded communities it is abstraction — their interests exist in the doctrine only when a judge invokes them. The scholar seat observes that all four descriptions are accurate simultaneously, which is precisely the signature the engine should detect from the structural data rather than from this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The judiciary sits near the beneficiary pole: it collects the arrangement's principal yield (accumulated interpretive authority) and controls its administration. Claimants sit low-d: subsidized by access. Value-conflicted speakers derive toward the beneficiary side from their beneficiary declaration, but their payer position is real — they finance the uncertainty — placing their true relationship near symmetric. Unpopular minority speakers sit near the full-target end, amplified by identity_locked exit: their expression is constitutive, so adverse weighings cannot be escaped by silence or relocation. Ordinary litigants derive high-d from victim status with constrained exit. Non-party communities are declared victims on the specific ground that the regime processes their interests without seating them — a cost the method itself imposes, distinct from the underlying speech harm. No directionality_overrides are authored: the derivation chain captures these relationships from the structural data, and because the override surface is keyed by power atom rather than agent, an override calibrated for the dual-positioned moderate speakers would misfire against the moderate claimants and litigants sharing that atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: novel speech conflicts keep arriving faster than categorical rules can absorb them, and the corroboration comes from outside the benefiting parties (legislative dockets, dissenting opinions conceding the hard cases, comparative scholarship). With founding_problem_status=live and disappearance_verdict=world_rearranges, the mismatch consumer finds no dead-mandate signal — this is not a zombie arrangement performing a retired function. The classification earns its keep by blocking two symmetric errors. Reading the regime as pure coordination ignores the measurable asymmetry: authority pools upward, uncertainty costs pool downward, and whole communities are weighed as abstractions. Reading it as pure extraction erases the function no rival currently replaces — abolish the weighing tomorrow and the hard cases do not disappear; they return unresolved. The tangled_rope verdict holds both truths in one structure, which is what the corpus needs from this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence,
    'This constraint is the balancing_reading instantiation of the speech_protection_boundary kernel; do the sibling readings (absolutist_reading, harm_limited_reading) instantiate constraints with materially different epsilon and classification over the same colloquial label?',
    'Author the sibling stories and compare engine-computed types and effective extraction across the family.',
    'If the siblings compute differently (expected: lower extraction for the absolutist reading, higher and speaker-borne for the harm_limited reading), the kernel contest is over WHICH constraint governs, and cross-reading comparison of this file''s metrics in isolation would be invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Committer structure: this story is one of three readings of the speech-protection kernel; sibling readings are separate constraints, not measurement parameters of this one.').

omega_variable(
    method_vs_discretion,
    'Is the multi-factor weighing apparatus a genuine decision procedure that constrains outcomes, or a legitimating frame over which panel composition effectively decides?',
    'Systematic coding of speech-case outcomes against factor profiles and panel composition; if the factors predict outcomes beyond panel identity, the method is doing real work.',
    'If largely post-hoc, theater_ratio is understated and the regime drifts toward theatrical maintenance of a discretion monopoly; if genuinely constraining, the coordination function is stronger than the authored metrics suggest and the extraction reading softens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_vs_discretion, empirical, 'Whether the announced weighing method disciplines outcomes or decorates discretion.').

omega_variable(
    chilling_effect_attribution,
    'How much observed speaker self-censorship is attributable to this regime''s unpredictability, as opposed to platform moderation, social sanction, and surveillance?',
    'Natural experiments where a speech domain moved between categorical and balancing treatment; difference-in-differences on speaker behavior across the transition.',
    'Resolves the suppression ambiguity: attribution to the regime raises effective suppression and pushes speaker seats toward the full-target end; attribution elsewhere lowers both and shrinks the victim class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_attribution, empirical, 'Attribution of chilling effects to the balancing method versus the surrounding speech environment.').

omega_variable(
    intermediate_scrutiny_consolidation,
    'Will intermediate scrutiny for coded speech and systemic-harm claims consolidate into predictable doctrine, or remain permanently case-by-case?',
    'Track doctrinal consolidation indicators — grant rates, circuit-split frequency, factor-list stabilization — over the coming decade.',
    'Consolidation would convert the arrangement''s uncertainty costs into settled expectations, pulling speaker seats back toward symmetry; permanent discretion sustains the current extraction profile and keeps the boundary structurally shift-prone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intermediate_scrutiny_consolidation, empirical, 'Stability trajectory of the intermediate-scrutiny layer that carries this reading''s distinctive structural delta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__balancing_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__balancing_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(spee_tr_t30, observed).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(spee_tr_t40, observed).
narrative_ontology:measurement(spee_tr_t50, speech_protection_boundary__balancing_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(spee_tr_t50, observed).
narrative_ontology:measurement(spee_tr_t60, speech_protection_boundary__balancing_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(spee_tr_t60, observed).
narrative_ontology:measurement(spee_tr_t70, speech_protection_boundary__balancing_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement_basis(spee_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__balancing_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__balancing_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(spee_be_t30, observed).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(spee_be_t40, observed).
narrative_ontology:measurement(spee_be_t50, speech_protection_boundary__balancing_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement_basis(spee_be_t50, observed).
narrative_ontology:measurement(spee_be_t60, speech_protection_boundary__balancing_reading, base_extractiveness, 60, 0.51).
narrative_ontology:measurement_basis(spee_be_t60, observed).
narrative_ontology:measurement(spee_be_t70, speech_protection_boundary__balancing_reading, base_extractiveness, 70, 0.48).
narrative_ontology:measurement_basis(spee_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__balancing_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__balancing_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(spee_su_t30, observed).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(spee_su_t40, observed).
narrative_ontology:measurement(spee_su_t50, speech_protection_boundary__balancing_reading, suppression_requirement, 50, 0.49).
narrative_ontology:measurement_basis(spee_su_t50, observed).
narrative_ontology:measurement(spee_su_t60, speech_protection_boundary__balancing_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement_basis(spee_su_t60, observed).
narrative_ontology:measurement(spee_su_t70, speech_protection_boundary__balancing_reading, suppression_requirement, 70, 0.42).
narrative_ontology:measurement_basis(spee_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how much speech protection the First Amendment provides' decomposes into three structurally distinct constraints sharing one kernel text. This file authors the balancing reading: contextual weighing, with moderate extraction centered on judicial discretion capture and speaker-side uncertainty costs. The absolutist reading (near-absolute protection, imminent-lawless-action exception) instantiates a low-extraction constraint; the harm_limited reading (protection conditional on absence of significant dignity/equality harm) instantiates a higher-extraction constraint whose costs fall on speakers and whose benefits concentrate in protected-class claimants. Each file carries its own epsilon over its own standing arrangement; they are linked because balancing opinions cite both the categorical tradition and the dignity-limit tradition as internal evidence, making the family edges load-bearing for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
