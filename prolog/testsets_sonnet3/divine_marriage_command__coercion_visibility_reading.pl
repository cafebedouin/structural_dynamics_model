% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: 1890 Manifesto as Acknowledged Response to Federal Coercion (Coercion-Visibility Reading)
 *   domain: religious_authority/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This story is one reading of a contested kernel — the 1890 Manifesto
 *   ending official church sanction of plural marriage. The
 *   coercion-visibility reading holds that the Manifesto is best understood
 *   as an acknowledged institutional response to escalating federal coercion
 *   (asset seizure under Edmunds-Tucker, disincorporation, mass
 *   disenfranchisement and imprisonment), and that its theological legitimacy
 *   is grounded, on this reading, in institutional survival necessity rather
 *   than in a revelatory event independent of that pressure. This is NOT a
 *   claim that the Manifesto lacks any revelatory framing; it is a claim
 *   about which causal story explains the timing and content of the change,
 *   evaluated by this reading's own lights. Two sibling readings exist as
 *   separate constraints: the continuationist reading (doctrine remains
 *   valid; Manifesto is prudential suspension, not rescission) and the
 *   substitutionist reading (monogamy is now doctrinally required; Manifesto
 *   is superseding new revelation). Those readings are not part of this file
 *   — see network links.
 *
 * KEY AGENTS:
 *   - church_governing_hierarchy: administers the shift and controls its ongoing narration (institutional/arbitrage) — collects the survival benefit
 *   - plural_marriage_practicing_families: bear the direct cost of dissolution/discipline (powerless/trapped)
 *   - excommunicated_fundamentalist_dissenters: bear long-run cost of continued suppression under the survival-necessity framing (powerless/trapped)
 *   - federal_government: applies the coercive pressure this reading identifies as proximate cause, then exits the story (institutional/analytical)
 *   - historians_and_church_archivists: analytical observer evaluating the documentary record independent of either party's interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "1890 Manifesto as Acknowledged Response to Federal Coercion (Coercion-Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '44d9a68a-31c5-46d2-a59a-bfce90849baf').
narrative_ontology:cs_kernel_codification('44d9a68a-31c5-46d2-a59a-bfce90849baf', fixed_text).
narrative_ontology:cs_authority_grounding('44d9a68a-31c5-46d2-a59a-bfce90849baf', lineage).
narrative_ontology:cs_interpretation_layer_present('44d9a68a-31c5-46d2-a59a-bfce90849baf').
narrative_ontology:cs_reading_relation('44d9a68a-31c5-46d2-a59a-bfce90849baf', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('44d9a68a-31c5-46d2-a59a-bfce90849baf', divine_marriage_command__substitutionist_reading, influences).
narrative_ontology:cs_axiom('44d9a68a-31c5-46d2-a59a-bfce90849baf', foundational, institutional_survival_constitutes_theological_warrant).
narrative_ontology:cs_axiom_status(institutional_survival_constitutes_theological_warrant, holdable).
narrative_ontology:cs_axiom_grounding('44d9a68a-31c5-46d2-a59a-bfce90849baf', institutional_survival_constitutes_theological_warrant, instrumental).
narrative_ontology:cs_axiom('44d9a68a-31c5-46d2-a59a-bfce90849baf', foundational, exogenous_coercion_is_valid_causal_input_to_doctrine).
narrative_ontology:cs_axiom_status(exogenous_coercion_is_valid_causal_input_to_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('44d9a68a-31c5-46d2-a59a-bfce90849baf', exogenous_coercion_is_valid_causal_input_to_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('44d9a68a-31c5-46d2-a59a-bfce90849baf', revelatory_continuity_doctrine).
narrative_ontology:cs_drift_state('44d9a68a-31c5-46d2-a59a-bfce90849baf', post_edmunds_tucker_crisis, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('44d9a68a-31c5-46d2-a59a-bfce90849baf', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, church_governing_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_continuity_project).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, plural_marriage_practicing_families).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, excommunicated_fundamentalist_dissenters).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, women_in_dissolved_plural_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, rank_and_file_monogamous_members).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_survival_as_theological_warrant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the Manifesto in 1890 under direct federal pressure (Edmunds-Tucker Act asset seizures, disincorporation, mass disenfranchisement, imprisonment of practicing members) and then administers its meaning going forward — deciding who is disciplined for continued plural marriage, how the change is narrated to members, and how church property and political standing are restored. Retains control of the doctrinal narrative even while acknowledging, in this reading, that the proximate cause was coercion rather than new revelation being sought independently of pressure.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, church_governing_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, church_governing_hierarchy, beneficiary).

% Already living in plural households at the time of the Manifesto; bear the direct cost of the institutional survival calculus — some marriages are quietly continued underground, others dissolved, families fractured, legal and social status thrown into instability. Cannot appeal the shift; it was made above them and imposed downward as a condition of the institution's continued existence.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, plural_marriage_practicing_families, payer,
    powerless, biographical, trapped, local).

% Those who continued to practice or advocate plural marriage after 1890 were disciplined, excommunicated, and in later decades treated as apostate splinter groups. On the coercion-visibility reading, their claim that the command was never actually rescinded by revelation gains theological teeth — but the institution treats their continued practice as the thing that must be suppressed to preserve the survival gain the Manifesto purchased.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, excommunicated_fundamentalist_dissenters, payer,
    powerless, generational, trapped, regional).

% Plural wives and their children absorbed the practical cost of family dissolution or forced secrecy following the Manifesto, with limited legal recourse, contested inheritance and legitimacy status, and no voice in the decision that reorganized their households as a byproduct of an institutional survival calculation made in response to external pressure.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, women_in_dissolved_plural_households, payer,
    powerless, biographical, trapped, local).

% Applied the coercive pressure (asset seizure, disincorporation, criminal prosecution) that this reading identifies as the actual proximate cause of the doctrinal shift, then largely exits the story once statehood and normalization are achieved — its coercive role is acknowledged by this reading but the federal government has no ongoing stake in how the theological narrative resolves.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, excluded,
    institutional, immediate, analytical, national).

% The majority of members who were not practicing plural marriage benefit from the institution's restored legal standing, statehood, and social normalization following the Manifesto; they inherit an institution whose continued existence and respectability depended on the change, largely without needing to reckon with the coercion that produced it.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, rank_and_file_monogamous_members, beneficiary,
    moderate, generational, constrained, national).

% Examine the documentary record — private correspondence, federal court filings, church leadership diaries — to assess whether the Manifesto's stated revelatory grounds are corroborated independently of the coercion narrative, or whether the sequence of events shows institutional survival pressure as the operative cause.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, historians_and_church_archivists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Manifesto coordinates the institution's continued legal existence: it allows the church to retain incorporation, recover seized assets, achieve statehood, and normalize relations with federal authority — a genuine collective-action problem for an institution facing dissolution.
% TRANSFER_FUNCTION: Moves the cost of institutional survival onto the families and individuals who were already practicing plural marriage: their marriages, households, and legal standing are sacrificed (via dissolution, secrecy, or discipline) so that the institution as a whole can retain assets, legal recognition, and political standing.
% ABSENT_VOICES: The plural wives and children whose households were dissolved or forced underground had no voice in the leadership decision; the federal government's coercive role, central to this reading, is not narrated in the institution's own retrospective account, which emphasizes revelatory continuity instead.
% DISAPPEARANCE_RATIONALE: If the coercion-visibility reading were institutionally adopted in place of the revelation-centered narrative, the church's claim to unbroken prophetic authority would be destabilized — plural-marriage-practicing descendants and fundamentalist splinter groups would gain a theological argument that the change was policy under duress rather than binding revelation, reopening questions of doctrinal continuity that the institution has spent over a century treating as settled.
% FOUNDING_PROBLEM: The federal government's Edmunds-Tucker Act and related enforcement was dismantling the church as a legal and economic entity — seizing property, disincorporating the institution, disenfranchising members — and the Manifesto was issued to halt that dismantlement and preserve institutional existence.
% FOUNDING_PROBLEM_CORROBORATION: Federal court records, contemporaneous press accounts, and non-Mormon legal historians corroborate that asset seizure and disincorporation were actively underway and that the Manifesto's timing tracks the coercion, not an independent revelatory timeline; the church's own official history attributes the change to revelation received by the church president, a claim corroborated only from within the benefiting institution itself.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) and suppression (0.71) are both authored high because this reading holds that the institution's continued respectability was purchased at the direct expense of already-practicing plural families and later dissenters, and that maintaining the survival-necessity narrative requires ongoing suppression of the continuationist counter-claim. Theater ratio rises steadily across the interval (0.15 to 0.62) because the institution's public framing increasingly emphasizes revelatory continuity even as, on this reading, the documentary record shows coercion as the operative variable — the theatrical gap widens as the coercive origin recedes from living memory and the revelation-centered narrative hardens into official history. accessibility_collapse (0.58) and resistance (0.55) are moderate: fundamentalist splinter groups persisting into the present show the alternative reading was never fully suppressed, only marginalized.
 *
 * DIRECTIONALITY LOGIC:
 *   church_governing_hierarchy is the structural beneficiary and agenda-setter: it collects the institutional survival gain (assets, statehood, legal normalization) and controls the narrative describing why the change occurred. plural_marriage_practicing_families, excommunicated_fundamentalist_dissenters, and women_in_dissolved_plural_households are targets: the survival calculus is paid for out of their household stability, legal standing, and, for dissenters, continued institutional membership. rank_and_file_monogamous_members sit as diffuse beneficiaries — they inherit the normalized institution without bearing the direct cost. federal_government is excluded from the ongoing structure once its coercive role is spent; it has no stake in how the theology resolves, which is precisely what this reading foregrounds and the institution's own history tends to background.
 *
 * MANDATROPHY ANALYSIS:
 *   The coercion-visibility reading treats the Manifesto's genuine coordination function (restoring legal existence, averting institutional dissolution) as real and distinct from the extraction it also enables (imposing the cost of that survival onto a subset of already-vulnerable members while narrating the shift as untethered from the coercion that produced it). Classifying this as tangled_rope rather than pure snare preserves the fact that institutional survival was a legitimate problem to solve; classifying it as tangled_rope rather than pure rope registers that the solution was not cost-free and that specific parties bore an asymmetric burden that the institution's official narrative subsequently minimizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelatory_independence_from_coercion,
    'Did the church president''s stated revelation regarding the Manifesto occur independently of the federal coercive pressure, or was the coercion itself the operative cause with revelatory language supplied after the fact to preserve authority continuity?',
    'Comparative analysis of the documentary timeline: private correspondence and diary entries preceding the Manifesto''s public announcement, cross-referenced against the escalation timeline of Edmunds-Tucker enforcement actions, to determine whether revelatory claims predate or postdate the coercive pressure becoming existential.',
    'If revelatory claims can be shown to substantially predate the coercive crisis, this reading''s core premise weakens and the substitutionist reading gains support. If the timeline shows revelatory language emerging only as coercion peaked, the coercion-visibility reading''s central claim is strengthened and the institution''s official narrative is more clearly a legitimacy-preserving construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelatory_independence_from_coercion, empirical, 'Whether revelatory grounding for the Manifesto is independent of or produced by the coercive crisis.').

omega_variable(
    non_revelatory_grounds_legitimacy_crisis,
    'If the institution''s authority structure formally admitted that a core doctrinal shift was grounded in exogenous coercion rather than revelation, would that admission destabilize the authority structure''s claim to unbroken prophetic continuity more broadly, beyond this single doctrine?',
    'Examine analogous cases where the institution has (or has not) publicly acknowledged non-revelatory causation for doctrinal change (e.g., the 1978 priesthood revelation and its own contested coercion-adjacent readings) and assess whether such acknowledgments have historically produced broader legitimacy crises or have been successfully contained.',
    'If acknowledgment is contained without broader crisis, the coercion-visibility reading can be accommodated without threatening the authority structure generally. If acknowledgment historically triggers cascading doctrinal reassessment, this reading carries higher structural stakes than its narrow subject matter suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_revelatory_grounds_legitimacy_crisis, conceptual, 'Whether admitting coercion as valid causal input to one doctrine threatens the authority structure''s general claim to revelatory continuity.').

omega_variable(
    committer_framing_alternative,
    'Is the coercion-visibility framing itself contestable at a deeper level — could the same documentary record support a reading where coercion and revelation are not exclusive causes but jointly sufficient (the institution sought and received guidance specifically about how to respond to coercion, making coercion the occasion rather than the substitute for revelation)?',
    'This would require a fourth reading distinguishing ''coercion as occasion for authentic revelation'' from both continuationist, substitutionist, and this coercion-as-cause reading — not resolvable within this story''s own structure, which authors coercion as the operative variable by the reading''s own lights.',
    'If a joint-causation reading is more defensible, this story''s ε (0.68) may overstate extraction by treating institutional survival as the sole warrant rather than one input among several; the classification would likely soften toward rope if genuine revelatory content is judged substantially present alongside the coercive occasion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether coercion-as-cause and revelation-as-cause are the only two available framings, or whether a joint-causation framing exists that this reading does not capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1862, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1862, 0.15).
narrative_ontology:measurement(divi_tr_t1874, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1874, 0.22).
narrative_ontology:measurement(divi_tr_t1882, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1882, 0.35).
narrative_ontology:measurement(divi_tr_t1887, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1887, 0.48).
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.55).
narrative_ontology:measurement(divi_tr_t1896, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1896, 0.6).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1904, 0.62).

% Extraction over time
narrative_ontology:measurement(divi_be_t1862, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1862, 0.28).
narrative_ontology:measurement(divi_be_t1874, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1874, 0.4).
narrative_ontology:measurement(divi_be_t1882, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1882, 0.55).
narrative_ontology:measurement(divi_be_t1887, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1887, 0.63).
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.68).
narrative_ontology:measurement(divi_be_t1896, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1896, 0.66).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1904, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1862, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1862, 0.2).
narrative_ontology:measurement(divi_su_t1874, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1874, 0.32).
narrative_ontology:measurement(divi_su_t1882, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1882, 0.5).
narrative_ontology:measurement(divi_su_t1887, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1887, 0.62).
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(divi_su_t1896, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1896, 0.71).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1904, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__coercion_visibility_reading, 0.1).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, substitutionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the divine_marriage_command kernel. continuationist_reading holds the doctrine remains valid and the Manifesto is prudential suspension under duress (ε likely lower — the coordination function is framed as temporary accommodation, not permanent surrender, though suppression of continued practitioners may still be substantial). substitutionist_reading holds monogamy is now doctrinally required via superseding revelation (ε likely much lower — the reading treats the change as legitimate doctrinal development with no coercion-derived extraction to register). This coercion_visibility_reading authors the highest ε of the three because it identifies the founding problem as external coercion rather than internal doctrinal development, which this reading treats as the least flattering and most contested account from the institution's own perspective. All three share the same historical episode but different beneficiary/victim structures and different ε — they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__coercion_visibility_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
