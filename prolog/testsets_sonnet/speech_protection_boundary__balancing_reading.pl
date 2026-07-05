% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Case-by-Case Balancing Reading of the Speech Protection Boundary
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the BALANCING reading of the
 *   speech-protection-boundary kernel: courts determine what speech is
 *   protected through case-by-case weighing of First Amendment interests
 *   against other constitutional values (equality, dignity, safety) and
 *   demonstrated harm, rather than through a near-absolute categorical rule
 *   (the absolutist reading) or a harm-conditioned threshold rule (the
 *   harm-limited reading). This is a distinct constraint from its siblings,
 *   not a different observable on the same one: the balancing reading has its
 *   own ε, its own beneficiary/victim structure (judicial discretion as the
 *   load-bearing institution, unpredictability as the primary cost), and its
 *   own classification. It is linked to the sibling readings via network only
 *   insofar as they compete for the same kernel; the sibling constraints are
 *   authored separately.
 *
 * KEY AGENTS:
 *   - reviewing_judiciary: sets and administers the balancing standard case by case (institutional/analytical)
 *   - targeted_harm_claimants: gain a legal avenue for harm-based claims unavailable under stricter rules (moderate/constrained)
 *   - constitutional_scholarship_apparatus: sustains interpretive labor market around doctrinal complexity (organized/arbitrage)
 *   - marginal_speakers_facing_unpredictable_liability: bear ex ante uncertainty costs, frequently self-censor (powerless/trapped)
 *   - activist_and_dissident_speakers: disproportionately exposed to harm-framing mobilized against disfavored speech (powerless/constrained)
 *   - platform_moderators_absorbing_legal_risk: apply an unsettled standard prospectively under liability exposure (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.42).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.48).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Case-by-Case Balancing Reading of the Speech Protection Boundary").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '683c67f5-085b-4e80-85a5-9c940a21f23f').
narrative_ontology:cs_kernel_codification('683c67f5-085b-4e80-85a5-9c940a21f23f', fixed_text).
narrative_ontology:cs_authority_grounding('683c67f5-085b-4e80-85a5-9c940a21f23f', lineage).
narrative_ontology:cs_interpretation_layer_present('683c67f5-085b-4e80-85a5-9c940a21f23f').
narrative_ontology:cs_reading_relation('683c67f5-085b-4e80-85a5-9c940a21f23f', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('683c67f5-085b-4e80-85a5-9c940a21f23f', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('683c67f5-085b-4e80-85a5-9c940a21f23f', foundational, constitutional_values_must_be_weighed_not_ranked).
narrative_ontology:cs_axiom_status(constitutional_values_must_be_weighed_not_ranked, holdable).
narrative_ontology:cs_axiom_grounding('683c67f5-085b-4e80-85a5-9c940a21f23f', constitutional_values_must_be_weighed_not_ranked, conventional).
narrative_ontology:cs_axiom('683c67f5-085b-4e80-85a5-9c940a21f23f', foundational, context_sensitivity_outperforms_categorical_rules_for_novel_harm).
narrative_ontology:cs_axiom_status(context_sensitivity_outperforms_categorical_rules_for_novel_harm, holdable).
narrative_ontology:cs_axiom_grounding('683c67f5-085b-4e80-85a5-9c940a21f23f', context_sensitivity_outperforms_categorical_rules_for_novel_harm, instrumental).
narrative_ontology:cs_reference_frame('683c67f5-085b-4e80-85a5-9c940a21f23f', post_brandenburg_categorical_settlement).
narrative_ontology:cs_drift_state('683c67f5-085b-4e80-85a5-9c940a21f23f', contemporary_platform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('683c67f5-085b-4e80-85a5-9c940a21f23f', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, reviewing_judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, targeted_harm_claimants).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, constitutional_scholarship_apparatus).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, marginal_speakers_facing_unpredictable_liability).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, activist_and_dissident_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, platform_moderators_absorbing_legal_risk).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, living_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts apply multi-factor balancing tests (intermediate scrutiny, context-sensitive harm assessment) case by case rather than through fixed categorical rules. This concentrates the power to determine which speech is protected in judicial discretion, exercised anew in each dispute, and the judiciary's continued centrality is itself a product of the doctrine it administers.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, reviewing_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and groups who can plausibly allege demonstrated harm from speech (harassment, incitement-adjacent conduct, coded threats) gain a legal avenue closed to them under a purely categorical or absolutist rule. They benefit when a court is willing to weigh their injury against the speaker's interest, but that benefit is contingent on prevailing in an unpredictable weighing exercise each time.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, targeted_harm_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Legal academics, appellate litigators, and doctrinal commentators sustain careers and institutional standing by elaborating, refining, and contesting balancing frameworks (levels of scrutiny, multi-factor tests). The complexity and case-specificity of the doctrine is itself a resource that generates ongoing demand for their interpretive labor.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_scholarship_apparatus, beneficiary,
    organized, generational, arbitrage, national).

% Individuals without resources for sustained litigation cannot reliably predict, in advance, whether their speech will be protected, because the boundary is set case-by-case rather than by a bright-line rule. They bear the cost of ex ante uncertainty and often self-censor rather than risk an adverse balancing outcome they cannot afford to appeal.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, marginal_speakers_facing_unpredictable_liability, payer,
    powerless, biographical, trapped, national).

% Speakers challenging powerful institutions or unpopular consensus positions are disproportionately exposed to claims that their speech causes 'demonstrated harm' to competing constitutional values, since harm framing can be mobilized against disfavored content more easily than against comfortable orthodoxy. Their exit is constrained by the same unpredictability that burdens marginal speakers generally, compounded by targeted enforcement patterns.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, activist_and_dissident_speakers, payer,
    powerless, biographical, constrained, national).

% Content moderation staff and compliance teams at platforms and institutions must apply the balancing standard prospectively, without the benefit of a completed judicial weighing, and bear liability or institutional risk for miscalibrating the line. They cannot exit the obligation to moderate but have no reliable rule to apply.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, platform_moderators_absorbing_legal_risk, payer,
    moderate, immediate, constrained, national).

% Advocates for a near-categorical free speech rule (Brandenburg-style imminent-lawless-action standard) participate in the same doctrinal debate but do not control the balancing framework's operation; their preferred bright-line rule is treated as one input to be weighed rather than the governing standard, and they experience each balancing decision as an erosion of the categorical protection they would prefer.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_reading_advocates, excluded,
    organized, generational, mobile, national).

% Advocates who would condition speech protection more strictly on absence of dignitary or equality harm argue the balancing reading still protects too much harmful speech by treating harm as one factor among several rather than a threshold condition. They are present in public discourse but the doctrine as administered does not adopt their threshold framing.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, harm_limited_reading_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating genuine conflicts between speech interests and other constitutional values (equality, dignity, public safety) in novel factual situations that neither absolute protection nor absolute harm-conditioning can anticipate in advance — courts can respond to context that categorical rules cannot.
% TRANSFER_FUNCTION: Moves the power to define protected speech's boundary from a fixed, publicly legible rule toward case-by-case judicial discretion; moves litigation and prediction costs from institutions with resources to speakers without them; moves some protective benefit to harm claimants who can access courts, at the expense of speakers who cannot predict outcomes in advance.
% ABSENT_VOICES: Absolutist-reading advocates and harm-limited-reading advocates are both present in scholarly and political discourse but neither controls the operative standard; low-resource speakers who cannot litigate to test the boundary are functionally absent from every individual balancing decision that sets precedent for speakers like them.
% DISAPPEARANCE_RATIONALE: If case-by-case balancing vanished overnight in favor of either sibling reading, the predictability of speech protection would shift sharply: an absolutist rule would remove most harm-based liability exposure for marginal and dissident speakers while foreclosing harm claimants' avenues; a harm-limited rule would expand harm claimants' leverage while chilling substantially more borderline speech. Either shift would visibly redistribute litigation outcomes, chilling effects, and institutional moderation burdens — the world does not stay the same under any alternative reading.
% FOUNDING_PROBLEM: Categorical free-speech rules proved unable to handle genuinely novel harms (targeted harassment campaigns, coded incitement, algorithmically amplified speech) without either over-protecting clear harm or under-protecting legitimate dissent; balancing was adopted to let courts weigh competing constitutional interests case by case rather than pre-committing to one absolute rule.
% FOUNDING_PROBLEM_CORROBORATION: Sitting appellate judges and mainstream constitutional scholars attest the case-by-case need remains live, citing genuinely novel fact patterns (deepfakes, coordinated harassment) that resist categorical treatment. Civil liberties organizations and empirical legal scholars outside the judiciary and academy note that decades of balancing jurisprudence have produced substantial doctrinal drift and unpredictability without a corresponding demonstrated reduction in harm relative to more categorical regimes — this corroboration is independent of, and partly critical of, the beneficiary groups named above.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).
:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than high, because balancing genuinely does resolve novel disputes that categorical rules mishandle — the coordination function is real. But it rises over the interval as the accumulated body of balancing precedent grows more elaborate and less predictable, increasingly burdening low-resource speakers who cannot litigate to discover where the line sits for cases like theirs. Suppression (0.48) reflects the structural unpredictability itself functioning as a chilling mechanism — speakers curtail expression not because a rule forbids it but because no rule tells them whether it is forbidden. Theater ratio is modest (0.28) — the doctrine performs some legitimating complexity but the underlying adjudicative function is substantially real.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, case-by-case balancing looks like principled coordination — genuine, context-sensitive adjudication of competing constitutional values that no fixed rule could anticipate. From the seat of a speaker who cannot afford to litigate a borderline case, the same structure looks like an enforced, unpredictable extraction of the freedom to speak without ex ante clarity. The engine computes both from the same structural data; the divergence is not resolved by declaring one seat correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the agenda-setting seat: it administers and continually re-derives the standard, and its institutional centrality is reinforced by the doctrine's case-by-case structure (no alternative institution can substitute). Targeted harm claimants and the scholarship apparatus benefit — the former gain access to remedies unavailable under a categorical rule, the latter gain a permanent interpretive market. Marginal speakers, dissidents, and platform moderators bear the transfer: prediction costs, chilling effects, and liability risk fall disproportionately on those without resources to litigate the boundary in their own case. Exit options differ sharply by resource level even among nominally similar speakers, which is why powerless payers are marked trapped/constrained while the scholarship apparatus (equally 'affected' by doctrinal instability) treats that instability as arbitrage opportunity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (categorical rules failing on novel fact patterns) has a contested status: courts and mainstream scholars attest it remains live, citing new harm modalities; critics outside the judiciary and academy note the doctrine has expanded well past its original remit without matching harm-reduction evidence, suggesting partial mandatrophy — the mechanism increasingly serves the interpretive apparatus and judicial discretion itself rather than only the harms it was built to address. Classifying this as tangled_rope rather than snare or rope preserves the genuine coordination function (real novel disputes DO need adjudication) while registering the asymmetric extraction (unpredictability costs concentrated on the powerless) that a pure-rope reading would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_kernel_reading_choice,
    'Is case-by-case balancing the correct reading of the First Amendment kernel, or does it represent an unprincipled drift away from either the absolutist Brandenburg standard or a harm-limited threshold rule?',
    'This is fundamentally a jurisprudential/political dispute, not an empirical one — resolvable only by tracking which reading commands durable majorities on reviewing courts over time and whether outcomes converge or diverge from either sibling reading''s predictions.',
    'If balancing is the correct reading, the moderate extractiveness measured here reflects genuine, irreducible complexity in adjudicating real constitutional conflicts. If balancing is itself a drift phenomenon — neither absolute nor harm-limited but an unprincipled hybrid that serves judicial and scholarly interests more than speakers on any side — the same metrics describe a Tangled Rope trending toward Piton (judiciary maintaining discretionary control past the point it resolves genuine ambiguity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balancing_kernel_reading_choice, conceptual, 'Whether the balancing reading is a principled resolution of genuine constitutional conflict or an unprincipled hybrid serving institutional interests.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the beneficiary/victim structure and ε change under the sibling absolutist or harm-limited readings, and what specific structural element do the three readings disagree about?',
    'The three readings are authored as separate constraint stories per the ε-invariance principle. The disagreement is located specifically in: (1) whether the protected/unprotected boundary is fixed ex ante (absolutist) or discovered ex post (balancing) or conditioned on a harm threshold (harm-limited); (2) which institution holds gatekeeper authority (categorical rule vs. distributed judicial discretion vs. harm-threshold adjudicators); (3) which party bears the unpredictability cost.',
    'Under the absolutist reading, marginal and dissident speakers would show far lower measured suppression/extractiveness (near-mountain territory for protected categories) while harm claimants would appear as the primary payers (unable to obtain remedies). Under the harm-limited reading, the beneficiary/victim sets would substantially invert: harm claimants would move toward beneficiary status with lower burden, while a much larger class of contested speech would move toward victim status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents where the three kernel readings structurally diverge and how ε and directionality would shift under each.').

omega_variable(
    judicial_discretion_capture_ambiguity,
    'Does the judiciary''s central role in balancing reflect neutral institutional competence, or does the balancing framework''s inherent complexity function to entrench judicial (and appellate-litigator) authority beyond what adjudicating genuine conflicts requires?',
    'Compare case outcomes and doctrinal complexity growth against a counterfactual baseline of harm rates and dispute novelty; if doctrinal complexity is growing faster than the rate of genuinely novel fact patterns requiring new balancing tests, that supports an entrenchment reading.',
    'If entrenchment, the tangled_rope classification understates victim-side extraction and the constraint trends toward genuine snare status for low-resource speakers over long time horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_capture_ambiguity, empirical, 'Whether judicial centrality in the balancing framework is functional necessity or self-reinforcing institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(spee_tr_t8, speech_protection_boundary__balancing_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(spee_tr_t16, speech_protection_boundary__balancing_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__balancing_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(spee_tr_t32, speech_protection_boundary__balancing_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(spee_be_t8, speech_protection_boundary__balancing_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(spee_be_t16, speech_protection_boundary__balancing_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__balancing_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(spee_be_t32, speech_protection_boundary__balancing_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(spee_su_t8, speech_protection_boundary__balancing_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(spee_su_t16, speech_protection_boundary__balancing_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__balancing_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(spee_su_t32, speech_protection_boundary__balancing_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the First Amendment speech protection boundary' per the ε-invariance principle. Each sibling reading (absolutist, balancing, harm_limited) is authored as an independent constraint with its own ε, beneficiary/victim structure, and classification, because measuring the boundary through each reading's operative test yields genuinely different extraction profiles and victim sets — not the same constraint viewed from different angles. All three are linked via affects_constraints because they compete for the same kernel text and jurisprudential authority; a shift in which reading commands majority support on reviewing courts directly reduces the operative scope of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
