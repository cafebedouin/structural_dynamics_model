% ============================================================================
% CONSTRAINT STORY: victim_self_attribution_foreclosure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_victim_self_attribution_foreclosure, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: victim_self_attribution_foreclosure
 *   human_readable: Inherited Diagnostic Vocabulary Foreclosing Outward Report of Domestic Harm
 *   domain: social epistemology / domestic power / diagnostic systems
 *
 * SUMMARY:
 *   Marfa Osipovna's body is depleted by conditions administered inside her
 *   household by her husband, Foma Silovich. She interprets each episode of
 *   collapse through a vocabulary — constitutional weakness, thin blood,
 *   moral fault — that she inherited long before the marriage, from mother,
 *   physician, and confessor. This vocabulary is causally prior to and
 *   independent of the household mechanism now harming her; it did not arise
 *   to conceal this particular harm. But its structure has the effect of
 *   foreclosing the one act that could redirect scrutiny outward: a report
 *   naming the household as the site of wrongness. She cannot make that
 *   report because, by every measure available to her interior experience,
 *   the house is good — he is not cruel to her in any register her vocabulary
 *   equips her to recognize as cruelty. The constraint is the vocabulary
 *   itself, not the household conditions directly; it is claimed tangled_rope
 *   because it does real coordination work (making suffering speakable across
 *   medicine, religion, and family without requiring formal accusation) while
 *   simultaneously enabling asymmetric extraction (the cost of household
 *   conditions lands entirely on her because the same vocabulary that makes
 *   her suffering legible also makes the household unindictable).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(victim_self_attribution_foreclosure, 0.72).
domain_priors:suppression_score(victim_self_attribution_foreclosure, 0.68).
domain_priors:theater_ratio(victim_self_attribution_foreclosure, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(victim_self_attribution_foreclosure, extractiveness, 0.72).
narrative_ontology:constraint_metric(victim_self_attribution_foreclosure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(victim_self_attribution_foreclosure, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(victim_self_attribution_foreclosure, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(victim_self_attribution_foreclosure, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(victim_self_attribution_foreclosure, tangled_rope).
narrative_ontology:human_readable(victim_self_attribution_foreclosure, "Inherited Diagnostic Vocabulary Foreclosing Outward Report of Domestic Harm").
narrative_ontology:topic_domain(victim_self_attribution_foreclosure, "social epistemology / domestic power / diagnostic systems").

domain_priors:requires_active_enforcement(victim_self_attribution_foreclosure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(victim_self_attribution_foreclosure, foma_silovich).
narrative_ontology:constraint_victim(victim_self_attribution_foreclosure, marfa_osipovna).
narrative_ontology:constraint_vindicates(victim_self_attribution_foreclosure, constitutional_weakness_doctrine).
narrative_ontology:constraint_vindicates(victim_self_attribution_foreclosure, thin_blood_etiology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experiences chronic depletion and periodic collapse whose proximate cause is conditions inside the household she shares with Foma Silovich. She interprets each episode through an inherited vocabulary — constitutional weakness, thin blood, moral fault of her own nervous constitution — acquired long before this marriage, from mother and physician and priest alike. This vocabulary is not a rationalization she constructs after the fact; it is the only interpretive frame she has ever possessed for bodily suffering. Because the frame locates the cause inside her own deficient nature, the single act that could redirect scrutiny outward — naming the house, naming him, to anyone outside it — never becomes thinkable, because she experiences the house, honestly and by every measure available to her, as good: he does not raise his voice, he does not withhold, he asks after her health with what reads to her as tenderness. To report would be to indict a home she has no vocabulary for indicting.
narrative_ontology:constraint_stakeholder(victim_self_attribution_foreclosure, marfa_osipovna, payer,
    powerless, biographical, identity_locked, local).

% Administers the household's daily conditions — diet, warmth, exertion, medical consultation — in ways that produce her depletion, without ever needing to conceal this from her, because her own vocabulary does the concealment for him. He need not suppress a report; the report is never formed. He can leave the marriage, remarry, or relocate at will; his exit options are unconstrained by anything the constraint does to him. He may not consciously understand himself as the cause — the vocabulary forecloses his own diagnostic clarity as thoroughly as hers — but he is the party who benefits from the household's conditions being read as her nature rather than his conduct.
narrative_ontology:constraint_stakeholder(victim_self_attribution_foreclosure, foma_silovich, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(victim_self_attribution_foreclosure, foma_silovich, agenda_setter).

% Confirms the constitutional-weakness diagnosis when consulted, using the same vocabulary in its professionalized form. He administers and legitimizes the interpretive frame without needing personal motive to harm her; the diagnostic category he was trained in and the household's convenience align without collusion. His authority makes the vocabulary harder to displace once she has heard it confirmed by a credentialed outside voice.
narrative_ontology:constraint_stakeholder(victim_self_attribution_foreclosure, attending_physician, agenda_setter,
    institutional, biographical, analytical, regional).

% Receives her confessions of suffering and frames them within a moral-fault register — that suffering is instructive, deserved, or purifying — reinforcing the self-attributive frame from a second institutional direction. He has no material stake in the household's conditions but supplies the oldest layer of the vocabulary she carries.
narrative_ontology:constraint_stakeholder(victim_self_attribution_foreclosure, priest_confessor, agenda_setter,
    institutional, generational, analytical, regional).

% Mother and aunts who could, in principle, hear a report and corroborate or contest it, but are never approached, because the report never forms enough to be spoken. They themselves largely share the same inherited vocabulary and might, if approached, redirect her back onto self-blame rather than outward — their exclusion is partly structural (she does not speak) and partly a property of what they would likely say if she did.
narrative_ontology:constraint_stakeholder(victim_self_attribution_foreclosure, female_relatives, excluded,
    powerless, biographical, constrained, local).

% Studies the case retrospectively, cross-referencing what interior diaries or confessional records reveal about her private vocabulary against the total absence of any outward report naming the household. Sees the coordination function (a shared, historically transmitted way of making suffering meaningful without requiring anyone to accuse anyone) and the extraction function (that the same vocabulary specifically forecloses the one report that would relocate liability onto the household) as operating through the identical mechanism.
narrative_ontology:constraint_stakeholder(victim_self_attribution_foreclosure, social_epistemologist_observer, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(victim_self_attribution_foreclosure, foma_silovich).
narrative_ontology:fixing_cost_class(victim_self_attribution_foreclosure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The inherited vocabulary of constitutional weakness, thin blood, and moral fault gives a household, a physician, and a confessor a shared, socially legitimate way to render female suffering meaningful and manageable without requiring anyone to formally accuse anyone else — it coordinates interpretation across medicine, religion, and domestic life so that suffering does not have to be litigated case by case.
% TRANSFER_FUNCTION: Moves the cost of the household's actual conditions — depletion, collapse, foreshortened vitality — from the party administering those conditions (Foma Silovich) onto the party experiencing them (Marfa Osipovna), by ensuring the causal narrative terminates inside her own body and character rather than at the household's threshold.
% ABSENT_VOICES: Female relatives who might corroborate a report if one were ever spoken are absent not because they are barred from the room but because the report that would summon them never forms; if a report did form, some of them might still redirect it back onto self-blame, since they carry overlapping fragments of the same vocabulary.
% DISAPPEARANCE_RATIONALE: If the inherited vocabulary vanished overnight and were replaced by a frame permitting outward causal attribution, Marfa Osipovna's interior monologue would begin locating episodes of collapse in household conditions rather than in her own constitution; the physician's and confessor's authority to close the question with a constitutional diagnosis would weaken; and the possibility of an outward report — the single act the constraint exists to foreclose — would open for the first time. The household's stability, which currently rests partly on the report never being spoken, would become contestable.
% FOUNDING_PROBLEM: The vocabulary was built, generations back, to give sufferers of unexplained female illness a legible, non-accusatory account of their condition at a time when female interior experience had few other socially sanctioned channels of description — it solved the problem of making suffering speakable at all within institutions (medicine, church, family) that had no other vocabulary to offer.
% FOUNDING_PROBLEM_CORROBORATION: The physician and confessor, both benefiting from the vocabulary's continued authority, attest the constitutional/moral-fault framing remains medically and spiritually sound — a live diagnostic and pastoral category, not an inherited fiction. Marfa Osipovna herself, from inside the frame, cannot corroborate its obsolescence since the frame forecloses her capacity to see it as constructed. No party outside the beneficiary and administering set is on record; the social-epistemologist observer's retrospective reading — that the founding problem (giving suffering a speakable frame) has been substantially solved by other means elsewhere while this household's version of the vocabulary now serves chiefly to foreclose outward attribution — is the only corroboration from outside the benefiting parties, and it arrives only after the fact.
narrative_ontology:disappearance_verdict(victim_self_attribution_foreclosure, world_rearranges).
narrative_ontology:founding_problem_status(victim_self_attribution_foreclosure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(victim_self_attribution_foreclosure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-17',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(victim_self_attribution_foreclosure, 'none', 1).
narrative_ontology:epsilon_provenance(victim_self_attribution_foreclosure, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(victim_self_attribution_foreclosure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(victim_self_attribution_foreclosure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(victim_self_attribution_foreclosure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval (0.48 to 0.72) as depletion accumulates and the vocabulary's foreclosure function compounds — each unreported episode further entrenches the frame that produced the non-report, a self-reinforcing loop rather than a static state. Suppression is substantial (0.68 at endpoint) but I read it as primarily internalized rather than externally enforced: no one is holding her mouth shut; the vocabulary itself performs the foreclosure from inside her own interpretive apparatus. Accessibility collapse is very high (0.81) because once she has fully internalized the constitutional-weakness frame, the alternative interpretation — that the house is implicated — becomes not merely difficult but structurally unavailable to her; there is no vocabulary slot for it. Resistance is low (0.15) precisely because resistance requires an alternative frame to resist from, and she has none. Theater ratio is modest and rising slowly (0.12 to 0.22): the physician's and confessor's confirmations are not pure performance, they are genuine (if wrong) diagnostic and pastoral activity, but an increasing share of that activity functions to re-seal the foreclosure each time symptoms recur.
 *
 * PERSPECTIVAL GAP:
 *   From Marfa Osipovna's own interior seat, this is not extraction at all — it is simply an account of her own nature, confirmed by trusted external authorities, inside a marriage she experiences as good. From the observer's seat, cross-referencing the total absence of outward report against the private vocabulary of self-blame, the same structure reads as a tangled rope: real coordination (suffering rendered speakable and socially manageable) fused to real extraction (the specific asymmetry of who bears the cost and who is protected from scrutiny). The engine should compute these as genuinely different per-seat classifications from the same structural facts, not as one seat being 'wrong' about the facts — she is not wrong about what she experiences; she lacks the vocabulary slot for a different causal account.
 *
 * DIRECTIONALITY LOGIC:
 *   Foma Silovich is the structural beneficiary: the household conditions he administers produce her depletion, and the vocabulary ensures the causal account never terminates at his conduct. His exit options are unconstrained (mobile) — nothing about the constraint traps him. Marfa Osipovna is the target: her exit options are identity_locked rather than merely trapped, because the foreclosure is not (primarily) that she is physically barred from leaving or reporting, but that her very identity — her sense of what is true about her own body and her own marriage — is constituted by the vocabulary that prevents the report. She cannot report the house because reporting the house would require her to disbelieve something she takes to be self-evidently true about her own constitution and about him. The physician and confessor are agenda_setters in the sense that they administer and legitimize the vocabulary, but they are not concentrated beneficiaries in the extractive sense the tangled-rope structure requires of foma_silovich — their gain is professional/institutional continuity, not the specific transfer from her body to his household.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving unexplained female suffering a legible, non-accusatory frame within institutions that offered no other vocabulary — was, at some point, genuinely live; classifying this purely as a snare from the outset would erase the real coordination function the vocabulary once performed and may still perform for others outside this household. But inside this specific household, the founding problem's continued relevance is contested: the physician and confessor (administering beneficiaries of the vocabulary's institutional authority) attest it remains sound, while the observer's retrospective reading suggests the coordination function has been substantially superseded elsewhere (other frames for speaking about suffering now exist) even as this household's deployment of the old vocabulary now serves chiefly to foreclose outward attribution. Tangled_rope, rather than snare, captures the diagnosis that the coordination function was and partly remains real, while the extraction riding on it has grown to dominate the structure's actual operation in this specific case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vocabulary_causal_priority,
    'Is the self-attributive vocabulary genuinely causally prior to and independent of the household mechanism (an inherited frame that happens to foreclose reporting), or was it partially shaped or reinforced by Foma Silovich''s conduct within the marriage in ways that make it less independent than it appears?',
    'Comparison of the vocabulary''s content and intensity before and after the marriage began, via correspondence, diary entries, or testimony from family members who knew her premaritally.',
    'If genuinely prior and independent, the tangled-rope reading (real coordination function predating and outside this specific extraction) is well-supported. If substantially shaped by the marriage itself, the constraint drifts toward snare — the coordination story becomes cover constructed contemporaneously with the extraction rather than an inherited structure the extraction merely exploits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vocabulary_causal_priority, empirical, 'Whether the self-blame vocabulary predates and is independent of the specific marriage, or was partly produced by it.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (she has literally no one to report to, no institutional channel that would hear her) or internalized (channels exist but she cannot use them because her own vocabulary forecloses the thought)?',
    'Post-hoc examination of whether any of the female relatives, if directly and explicitly asked whether the household could be implicated, would have been willing to hear and act on such a report — distinguishing an absent channel from an unusable one.',
    'If purely internalized, the effective suppression is higher than the structural measure suggests, because removing external barriers (were any identified) would not open the report — she would carry the foreclosure with her regardless of who was listening. If partly structural, external intervention (an outside party directly raising the possibility) could have material effect even without dismantling her vocabulary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism behind the absence of an outward report.').

omega_variable(
    physician_confessor_intent,
    'Do the physician and confessor knowingly reinforce a diagnostic frame they suspect is inaccurate because it is institutionally convenient, or do they hold the constitutional-weakness and moral-fault framings as genuine professional and pastoral truth?',
    'Records of the physician''s private case notes or the confessor''s private correspondence, if any survive, compared against their public/professional pronouncements to the family.',
    'If knowing reinforcement, they shift from incidental administrators of a shared cultural vocabulary toward active co-beneficiaries, strengthening the case for treating them as concentrated beneficiaries alongside Foma Silovich. If genuine belief, they remain agenda-setters without extraction, consistent with the current tangled-rope reading that names foma_silovich alone as the concentrated beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(physician_confessor_intent, conceptual, 'Whether institutional administrators of the vocabulary are knowing beneficiaries or genuine believers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(victim_self_attribution_foreclosure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vict_tr_t0, victim_self_attribution_foreclosure, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vict_tr_t4, victim_self_attribution_foreclosure, theater_ratio, 4, 0.14).
narrative_ontology:measurement(vict_tr_t8, victim_self_attribution_foreclosure, theater_ratio, 8, 0.17).
narrative_ontology:measurement(vict_tr_t12, victim_self_attribution_foreclosure, theater_ratio, 12, 0.19).
narrative_ontology:measurement(vict_tr_t16, victim_self_attribution_foreclosure, theater_ratio, 16, 0.21).
narrative_ontology:measurement(vict_tr_t20, victim_self_attribution_foreclosure, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(vict_be_t0, victim_self_attribution_foreclosure, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(vict_be_t4, victim_self_attribution_foreclosure, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(vict_be_t8, victim_self_attribution_foreclosure, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(vict_be_t12, victim_self_attribution_foreclosure, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(vict_be_t16, victim_self_attribution_foreclosure, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(vict_be_t20, victim_self_attribution_foreclosure, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(vict_su_t0, victim_self_attribution_foreclosure, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(vict_su_t4, victim_self_attribution_foreclosure, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(vict_su_t8, victim_self_attribution_foreclosure, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(vict_su_t12, victim_self_attribution_foreclosure, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(vict_su_t16, victim_self_attribution_foreclosure, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(vict_su_t20, victim_self_attribution_foreclosure, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(victim_self_attribution_foreclosure, identity_coordination).
narrative_ontology:boltzmann_floor_override(victim_self_attribution_foreclosure, 0.08).

% DUAL FORMULATION NOTE:
% This story isolates the vocabulary-as-foreclosure-mechanism from the household conditions themselves. A sibling story could be written for the household conditions as an independent constraint (e.g. domestic resource administration) with its own epsilon; that story would have Foma Silovich as agenda_setter/beneficiary and Marfa Osipovna as payer/victim through a different mechanism (material conditions rather than interpretive vocabulary). The two would share stakeholders but differ in what is causally prior — this story's epsilon is specific to the interpretive foreclosure, not to the underlying physical harm, and should not be merged with a story about the harm mechanism itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(victim_self_attribution_foreclosure, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
