% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor-Satisfaction Mechanism (Gradual Decline Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This story authors the DECLINE READING of the honor-satisfaction kernel:
 *   dueling as a practice whose frequency fell gradually across roughly a
 *   century, from a socially load-bearing mechanism to a fringe curiosity,
 *   without any sharp discontinuity in the underlying concept. Under this
 *   reading the constraint's ε and suppression both fall smoothly as
 *   frequency falls, while enforcement (criminal prosecution, changing
 *   military codes) rises to fill the gap left by declining voluntary
 *   compliance — the classic piton signature of an atrophying practice
 *   sustained increasingly by theater and formal statute rather than live
 *   social necessity. This is explicitly NOT the contraction reading (which
 *   holds dueling became a category-level cognitive impossibility, i.e.
 *   unthinkable rather than merely rare) and NOT the composite reading (which
 *   attributes the decline to several distinct co-operating mechanisms rather
 *   than a single frequency curve). Those are separate constraints, linked
 *   via network.affects_constraints, sharing the same kernel but authoring
 *   different ε trajectories and different structural claims about what
 *   actually happened to the practice.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_class: primary beneficiary of the status-sorting function, powerful/constrained
 *   - professional_officer_corps: secondary beneficiary and agenda-setter, retains formal codes longest
 *   - duel_participants: primary target, bears mortal risk, trapped exit at the point of challenge
 *   - duel_participants_dependents: secondary victims, powerless, bear costs with no voice
 *   - courts_and_legislatures: excluded/observer, formal objection long preceding effective enforcement
 *   - social_historians: analytical observer, reconstructs the frequency curve itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.38).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.42).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor-Satisfaction Mechanism (Gradual Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '74c709aa-37bb-491f-a779-0802c8baaabe').
narrative_ontology:cs_kernel_codification('74c709aa-37bb-491f-a779-0802c8baaabe', distributed).
narrative_ontology:cs_authority_grounding('74c709aa-37bb-491f-a779-0802c8baaabe', practice).
narrative_ontology:cs_interpretation_layer_present('74c709aa-37bb-491f-a779-0802c8baaabe').
narrative_ontology:cs_reading_relation('74c709aa-37bb-491f-a779-0802c8baaabe', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('74c709aa-37bb-491f-a779-0802c8baaabe', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('74c709aa-37bb-491f-a779-0802c8baaabe', foundational, honor_mechanism_continuity_through_frequency_change).
narrative_ontology:cs_axiom_status(honor_mechanism_continuity_through_frequency_change, holdable).
narrative_ontology:cs_axiom_grounding('74c709aa-37bb-491f-a779-0802c8baaabe', honor_mechanism_continuity_through_frequency_change, empirically_contingent).
narrative_ontology:cs_axiom('74c709aa-37bb-491f-a779-0802c8baaabe', secondary, single_channel_attrition_sufficient_explanation).
narrative_ontology:cs_axiom_status(single_channel_attrition_sufficient_explanation, holdable).
narrative_ontology:cs_axiom_grounding('74c709aa-37bb-491f-a779-0802c8baaabe', single_channel_attrition_sufficient_explanation, empirically_contingent).
narrative_ontology:cs_reference_frame('74c709aa-37bb-491f-a779-0802c8baaabe', aristocratic_honor_as_lived_practice).
narrative_ontology:cs_drift_state('74c709aa-37bb-491f-a779-0802c8baaabe', late_nineteenth_century_fringe_status, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('74c709aa-37bb-491f-a779-0802c8baaabe', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, professional_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duel_participants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duel_participants_dependents).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, honor_as_gradable_social_currency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the dueling code across the 18th-19th centuries as a marker distinguishing gentlemen from commoners, litigants, and the newly wealthy. Benefits from a mechanism that lets status disputes be settled without resort to courts that might expose them to bourgeois legal equality. As the century progresses, fewer members actually duel, but the class continues to invoke the code's existence as a marker of who counts as a gentleman at all.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_class, beneficiary,
    powerful, generational, constrained, national).

% Military codes of honor formally retain the duel or its ritual substitutes (courts of honor, formal apology rituals) far longer than civil society, because officer identity is partly constituted by willingness to answer for insult with the body. Sets and polices the informal rules of when a challenge is 'required.' Frequency of actual duels falls steeply even within this population over the measured interval, while the formal expectation persists.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, professional_officer_corps, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, professional_officer_corps, agenda_setter).

% A man publicly insulted, or accused of cowardice for declining a challenge, faces social death if he refuses and physical risk if he accepts. As frequency declines, the social cost of refusal also declines unevenly by region and class, so a participant's actual exposure depends heavily on local reference groups rather than any national norm. Pays with life, injury, legal jeopardy, or reputational ruin depending on era and locale.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duel_participants, payer,
    moderate, biographical, trapped, local).

% Wives, children, and other dependents of a killed or maimed duelist bear the economic and social consequences of a code they had no voice in setting. They cannot challenge, refuse on the participant's behalf, or claim compensation through the honor system itself; their only recourse, where it exists, is the ordinary civil or criminal courts increasingly available as the practice declines.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duel_participants_dependents, payer,
    powerless, biographical, trapped, local).

% Pass increasingly specific statutes criminalizing dueling and prosecute survivors and seconds with rising, though inconsistent, vigor across the interval. Their formal position (dueling is illegal) coexists for decades with widespread non-enforcement among elites, so their voice in the actual practice of honor disputes is present in law but largely excluded from the social mechanism that decides real cases.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, courts_and_legislatures, excluded,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, courts_and_legislatures, observer).

% Study the frequency curve of recorded duels, challenges, and near-duels across the interval, tracking the decline as gradual attrition rather than sudden collapse. Their reconstructions rely on incomplete records (many late duels were unreported to avoid prosecution), which is itself evidence for the decline-in-frequency reading over a sharp discontinuity.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, non-judicial procedure for resolving accusations that threaten a man's standing among peers, allowing honor disputes to be settled through a ritual both sides recognize as legitimate without escalating into open feud or endless litigation.
% TRANSFER_FUNCTION: Moves physical risk and reputational stakes from the offended party's ongoing social standing onto a single ritualized contest; costs (death, injury, prosecution, family ruin) fall on participants and their dependents while the aristocratic and officer classes retain the benefit of a functioning status-sorting mechanism, increasingly at declining frequency and declining individual cost as fewer instances actually occur.
% ABSENT_VOICES: Dependents of duelists, and the broader non-elite public who bore no comparable code but were bound by the same criminal statutes, had no say in when a challenge was 'required.' Courts and legislatures voiced formal objection through statute for decades before enforcement caught up with the declining but still-real social expectation.
% DISAPPEARANCE_RATIONALE: Under the decline reading, the mechanism's disappearance is itself the phenomenon being measured — it did not vanish overnight but thinned out over generations, with courts of honor, formal apologies, and libel suits absorbing the coordination function as actual armed combat became rarer. Whether 'the world rearranges' depends on which decade of the decline curve one asks: early in the interval, abolition would have been a shock; late in the interval, it merely certified what frequency data already showed.
% FOUNDING_PROBLEM: Aristocratic and military honor codes needed a settlement mechanism for status disputes that did not require submitting to increasingly bourgeois, equalizing courts, and that let a man demonstrate willingness to risk his life rather than accept an insult passively.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and criminal-court records from the period (outside the dueling class itself) attest that the underlying status-anxiety problem persisted long after dueling frequency collapsed, migrating into libel law, formal codes of conduct, and other substitute mechanisms — suggesting the founding problem outlived this particular constraint even as the constraint itself declined. Contemporary moralists and clergy, also outside the beneficiary class, corroborate the declining frequency but dispute whether the underlying need for the mechanism was ever legitimate rather than manufactured status theater.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness falls from 0.62 to 0.38 across the interval because the decline reading holds that fewer duels actually occur, so the total extraction (death, injury, family ruin) shrinks in aggregate even though any single late-era duel remains as lethal as an early one. Theater ratio rises correspondingly (0.20 to 0.55) because an increasing share of what remains of the code is performative — formal challenges issued and settled by apology, honor courts substituting for combat, published codes of conduct maintained more as social signaling than lived practice. Suppression (measured here as suppression_requirement, the enforcement side) rises from 0.30 to 0.72 as legislatures and courts increasingly criminalize and prosecute what social consensus was already abandoning voluntarily — enforcement intensifying precisely as the underlying practice weakens is the piton signature: the state expends more coercive effort maintaining a prohibition against a dying practice than the practice itself required to sustain.
 *
 * PERSPECTIVAL GAP:
 *   From the aristocratic/officer seat, the declining frequency reads as evidence of civilizational progress and voluntary moral improvement — the code simply falls into disuse because gentlemen increasingly resolve disputes otherwise. From the payer seat (participants and dependents), the same decline reads as survivors' luck distributed unevenly: the code's lethality for any individual unlucky enough to be challenged did not decline at all, only its statistical frequency did. The engine's per-seat computation should reflect this: the beneficiary seats classify the declining-frequency arrangement close to rope/piton (voluntary abandonment of a once-functional norm), while the payer seats classify any given instantiation close to snare (full-lethality risk imposed with no meaningful voice).
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic and officer-corps beneficiaries retain low directionality (near the beneficiary end) throughout the decline because their exposure is chosen and status-protective even as fewer of them actually fight; the code's mere existence, not its exercise, is what benefits them. Duel participants sit near the target end at any point they are actually challenged, with a trapped exit at the moment of challenge regardless of how rare that moment has become — declining frequency does not soften the directionality of any single instance. Dependents carry the highest effective target-directionality because they bear catastrophic cost with zero agency in triggering it and no formal standing within the honor mechanism to contest it.
 *
 * MANDATROPHY ANALYSIS:
 *   The decline reading resolves a mandatrophy question directly: the founding problem (status-anxiety resolution outside bourgeois courts) does not disappear when duel frequency falls — it migrates into substitute mechanisms (libel suits, courts of honor, professional codes of conduct) even as the literal dueling constraint becomes fringe. This prevents mislabeling the whole honor-satisfaction kernel as either 'solved' (contraction reading's stronger claim) or 'multiply replaced' (composite reading's stronger claim); the decline reading's contribution is narrower and more conservative: the SAME mechanism simply occurred less often, with rising enforcement filling the compliance gap, until what remained was mostly theater. mandatrophy_resolved is not declared here because, under this reading specifically, the mandate did not resolve — it thinned to fringe status while the underlying anxiety persisted unaddressed by this mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    frequency_decline_vs_conceptual_break,
    'Is the empirical record (falling duel counts over the interval) better explained by simple declining frequency of an unchanged practice, or does the record itself show a discontinuity consistent with the contraction reading''s claim that dueling became cognitively unthinkable at some point rather than merely rarer?',
    'Fine-grained analysis of the shape of the frequency curve: a smooth monotonic decline supports this reading; a sharp inflection point coinciding with a specific generational or legal shift would support contraction_reading instead. Qualitative sources (private letters, memoirs) describing whether dueling was still considered a live option late in the interval, versus described as archaic or absurd, would also discriminate between readings.',
    'If the record shows a sharp inflection rather than smooth decline, this story''s claimed_type and ε-trajectory would be the wrong model for the phenomenon, and the contraction_reading sibling would be the structurally accurate account instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frequency_decline_vs_conceptual_break, empirical, 'Whether the historical record supports gradual decline versus conceptual discontinuity.').

omega_variable(
    single_vs_multiple_mechanism,
    'Did a single mechanism (the honor code itself, simply invoked less often) account for the decline, or did several distinct mechanisms (criminal prosecution, insurance/liability law, bourgeois normative change, shifting definitions of honor) operate together, as the composite_reading claims?',
    'Comparative institutional history across jurisdictions with different legal, religious, and class structures: if decline correlates tightly with a single variable (e.g. enforcement intensity) across contexts, this reading is supported; if decline correlates with different variables in different contexts, the composite reading better fits the data.',
    'If multiple independent mechanisms are shown to operate, this story''s single-mechanism decline framing would need to be decomposed further, and part of what this story attributes to ''declining frequency'' would properly belong to the composite_reading sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_vs_multiple_mechanism, conceptual, 'Whether decline reflects one mechanism weakening or several distinct mechanisms operating in parallel.').

omega_variable(
    beneficiary_naturalization_ambiguity,
    'Do the aristocratic and officer-corps beneficiaries genuinely experience the code''s decline as voluntary moral progress, or is this a retrospective naturalization that obscures the constraint''s continued extractive potential for any individual actually challenged?',
    'Contemporary primary-source attitudes (letters, dueling manuals, editorials) from within the beneficiary class late in the interval, compared against the survival/casualty data for individuals actually challenged during the same period.',
    'If beneficiaries'' own contemporary accounts show continued acceptance of lethal risk as legitimate rather than embarrassment or reform sentiment, this weakens the ''progress'' framing and strengthens the piton/snare divergence noted in the perspectival gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_naturalization_ambiguity, conceptual, 'Whether beneficiary self-narratives of decline reflect genuine attitude change or retrospective naturalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__decline_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_mechanism__decline_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_mechanism__decline_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_mechanism__decline_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement(hono_tr_t80, honor_satisfaction_mechanism__decline_reading, theater_ratio, 80, 0.51).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_mechanism__decline_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(hono_be_t80, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(hono_su_t80, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__decline_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the honor_satisfaction_mechanism kernel, each authored as a structurally distinct story per the ε-invariance principle. decline_reading (this story) claims a single continuous mechanism whose incidence fell smoothly to fringe status, with ε and suppression both declining as theater_ratio rises — a piton-shaped trajectory. contraction_reading claims a category-level cognitive discontinuity (dueling became unthinkable, not merely rare) and would author a sharper ε drop concentrated near a specific historical inflection rather than a smooth curve. composite_reading claims multiple distinct mechanisms (state monopoly, bourgeois norms, insurance/liability, category-shift in honor's meaning) operated together and would author several partial ε-components rather than one aggregate trajectory. All three share the same underlying historical kernel and beneficiary/victim structure in outline but differ in claimed_type, ε-trajectory shape, and structural mechanism — hence three files, linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
