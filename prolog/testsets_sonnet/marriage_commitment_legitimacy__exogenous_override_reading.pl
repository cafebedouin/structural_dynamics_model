% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federally-Coerced Doctrinal Suspension
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates the exogenous-override reading of the
 *   marriage_commitment_legitimacy kernel: the 1890 Manifesto is read as a
 *   coerced practical suspension of plural marriage extracted from the LDS
 *   Church by escalating federal legal and economic pressure (Morrill Act
 *   1862, Edmunds Act 1882, Edmunds-Tucker Act 1887), with the underlying
 *   theological claim of D&C 132 left formally unrescinded. On this reading
 *   the constraint is a Tangled Rope: it possesses a surface coordination
 *   story (the Church 'submitting to the law' to preserve institutional
 *   viability and gain statehood) but the coordination is imposed by threat,
 *   not consented to, and there is a clear asymmetric extraction — the
 *   federal government and the anti-polygamy coalition get their policy
 *   objective; the Church hierarchy, general membership, and especially
 *   plural families bear the cost of an unresolved doctrinal rupture. This is
 *   NOT the same constraint as the endogenous-reinterpretation reading (which
 *   would classify closer to Rope/Mountain — genuine revelation resolving
 *   genuine tension) or the hybrid-pragmatic reading (Scaffold-flavored —
 *   strategic ambiguity managing a transition). Each reading has a different
 *   epsilon and belongs in its own file, linked by kernel_id.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.79).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.81).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "1890 Manifesto as Federally-Coerced Doctrinal Suspension").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '15e9f1b6-4f76-4098-bbd8-eb8e112747f8').
narrative_ontology:cs_kernel_codification('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', formalized).
narrative_ontology:cs_authority_grounding('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', extraction).
narrative_ontology:cs_interpretation_layer_present('15e9f1b6-4f76-4098-bbd8-eb8e112747f8').
narrative_ontology:cs_reading_relation('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', foundational, practice_suspension_under_duress_leaves_doctrine_intact).
narrative_ontology:cs_axiom_status(practice_suspension_under_duress_leaves_doctrine_intact, holdable).
narrative_ontology:cs_axiom_grounding('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', practice_suspension_under_duress_leaves_doctrine_intact, empirically_contingent).
narrative_ontology:cs_axiom('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', secondary, coerced_institutional_compliance_confers_no_theological_legitimacy).
narrative_ontology:cs_axiom_status(coerced_institutional_compliance_confers_no_theological_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', coerced_institutional_compliance_confers_no_theological_legitimacy, deontological).
narrative_ontology:cs_reference_frame('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', eternal_covenant_obligation_1852).
narrative_ontology:cs_drift_state('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', post_manifesto_1890, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('15e9f1b6-4f76-4098-bbd8-eb8e112747f8', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_political_coalition).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_general_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, plural_wives_and_children).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, fundamentalist_lds_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_1890).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passed the Edmunds-Tucker Act, disincorporated the Church, seized its property, disenfranchised polygamists, and imprisoned Church leadership until the practice of plural marriage was suspended. Holds the coercive machinery — asset seizure, criminal prosecution, statehood denial — that produced the Manifesto's timing and language. Collects institutional compliance and the removal of a politically destabilizing practice from the territory seeking statehood.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Faced dissolution of the corporate Church, confiscation of temples, and continued imprisonment of leadership under the Edmunds-Tucker Act. Issued the Manifesto announcing suspension of new plural marriages while continuing to frame the act publicly in providential language. From this reading, the hierarchy is simultaneously the entity administering the suspension and the party paying the cost of doctrinal retreat under duress — trapped between institutional survival and the prior revelatory claim of Doctrine and Covenants 132.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_1890, payer,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_1890, agenda_setter).

% Believing members who had accepted plural marriage as a binding celestial commandment now had that practice withdrawn without any corresponding claim that the underlying doctrine was false. They bear the cost of a widening gap between what they were taught was eternally required and what institutional survival now demanded; leaving the faith community meant losing kinship, economic, and social networks built around it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_general_membership, payer,
    moderate, generational, constrained, national).

% Existing plural families were left in ambiguous legal and social standing — some publicly maintained, many quietly continued, all now vulnerable to prosecution or social repudiation depending on enforcement discretion. Wives in non-first marriages had no independent legal recognition and no institutional advocate once the practice was suspended; children of plural unions bore inheritance and legitimacy uncertainty for a generation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, plural_wives_and_children, payer,
    powerless, biographical, trapped, regional).

% Held that the Manifesto could not abrogate a commandment given by revelation and continued or later revived plural marriage practice, splitting from the main institutional body. They argue from outside the benefiting parties that the exogenous-override reading is correct — the doctrine was never actually rescinded, only suspended under external force, and the mainstream church's later claim of continuing revelation is a retroactive legitimation of surrender. They are excluded from the institutional conversation and in some cases excommunicated for holding this reading.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, fundamentalist_lds_dissenters, excluded,
    powerless, civilizational, trapped, regional).

% National reformers, Protestant clergy networks, and congressional actors who campaigned against polygamy as a moral and political threat to the union achieved their policy objective: institutional abandonment of the practice, paving the way for Utah statehood on terms acceptable to the federal government. They benefit from the coercive campaign's success without bearing any of its enforcement costs.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_political_coalition, beneficiary,
    organized, biographical, mobile, national).

% Examine court records, Church correspondence, and the Manifesto's own text (which conspicuously does not claim new revelation, only that the Church 'submit to the law') to assess whether the 1890 declaration constitutes genuine doctrinal change or a coerced practical suspension recorded in language chosen to preserve institutional face.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, there is no genuine coordination function internal to the arrangement — the 'coordination' claimed (aligning Church practice with federal law to enable statehood and institutional survival) is imposed from outside via coercive threat, not solved by mutual agreement among the parties to the underlying doctrinal commitment.
% TRANSFER_FUNCTION: Moves institutional compliance and abandonment of a core practice from the Church and its plural-marriage-practicing members to the federal government and the anti-polygamy coalition, in exchange for cessation of asset seizure, prosecution, and the promise of eventual statehood — a transfer extracted under threat rather than negotiated as mutual benefit.
% ABSENT_VOICES: Plural wives and their children had no formal voice in either the federal legislative process or the Church's institutional decision to issue the Manifesto; fundamentalist dissenters who held the exogenous-override reading explicitly were excommunicated rather than heard, removing the clearest internal articulation of this very reading from the institutional record.
% DISAPPEARANCE_RATIONALE: Absent the federal coercive campaign (property seizure, disenfranchisement, imprisonment), the Church's practice of plural marriage would very likely have continued unchanged through the 1890s; the entire subsequent institutional history — statehood, mainstream assimilation, the excommunication of fundamentalist splinter groups, the doctrinal reframing of D&C 132 as no-longer-operative-but-still-true — depends on this coercive intervention having occurred.
% FOUNDING_PROBLEM: The federal government sought to eliminate plural marriage as a perceived threat to the political and moral order of the union, using economic and criminal coercion against the Church as an institution to force practice change where persuasion and earlier legislation (Morrill Act, 1862) had failed.
% FOUNDING_PROBLEM_CORROBORATION: Federal legal historians and constitutional scholars outside the LDS tradition (e.g., analyses of Edmunds-Tucker Act enforcement and Reynolds v. United States commentary) attest that the coercive campaign achieved its stated policy objective and was substantially wound down after 1890; fundamentalist LDS dissenters, also outside the mainstream Church's benefiting leadership, independently corroborate that the change was practice-only and doctrinally unresolved from their reading — though they draw the opposite normative conclusion from the historians.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 1862 through 1890 (0.35 to 0.79) tracking the escalating coercive machinery — disincorporation, property seizure, imprisonment — culminating at the Manifesto's issuance, then declines modestly as post-1890 enforcement relaxed, though it never returns to baseline because the doctrinal ambiguity itself becomes a persistent, lower-grade extraction (ongoing suppression of the fundamentalist reading). Theater ratio climbs steadily and plateaus high (0.62-0.63) because, on this reading, the Manifesto's providential public language and later 1904 Second Manifesto are read as performative cover maintaining the appearance of continuous revelation over what was materially a forced retreat. Suppression requirement peaks precisely at the coercive climax (1887-1890, ~0.81-0.88) and then falls as the enforcement apparatus was no longer needed once compliance was secured — this is the signature of externally-imposed coercion rather than self-sustaining internal norm enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's analytical seat, this looks like successful law enforcement against an aberrant practice — a closed, resolved matter. From the Church hierarchy's seat, it is an act of institutional survival under existential threat, later reframed publicly as ongoing revelation. From the fundamentalist dissenters' seat, it is exactly what this reading's label states: coercion, doctrinally unresolved, later papered over. The engine computing different per-seat classifications from the same structural data is the intended behavior — this is not resolved by picking a winner, it is resolved by generating this as one of three distinct sibling constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and the anti-polygamy coalition are structural beneficiaries: they achieve their policy objective and bear none of the institutional or familial cost. Church hierarchy sits in an unusual dual position — agenda_setter in issuing the Manifesto, but payer in the sense that institutional survival was purchased at the cost of an admitted-or-not doctrinal contradiction it must now manage indefinitely. Plural wives and children are the most powerless and trapped victims: no legal standing, no institutional voice, bearing the sharpest material and status costs. Fundamentalist dissenters are structurally excluded — the reading they hold (this reading) is the one the mainstream institution has the strongest incentive to suppress, since acknowledging it as correct would undermine claims of continuous revelation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — federal elimination of plural marriage as a perceived threat to the political order — is dead by 1904 in the sense that the federal coercive campaign fully achieved its object and was not renewed. But on this reading the arrangement's justification (the Manifesto's own text, submission to law rather than claimed revelation) never squarely resolved whether the underlying doctrine survived. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges is intentional: the coercive episode is over, but the institutional structure built atop the coerced compliance (correlated doctrine, excommunication of dissenters, the eventual 1904 reaffirmation) persists as an artifact of that historical coercion rather than as an independently justified arrangement — precisely the zombie-mandate pattern the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_versus_coercion_causal_priority,
    'Was the causal mechanism producing the 1890 Manifesto genuine prophetic revelation, federal coercion, or an inseparable combination — and can historical evidence adjudicate this, or is it an irreducibly theological question outside historical method''s reach?',
    'Close textual analysis of the Manifesto''s own language (which claims submission to law, not new revelation) against private correspondence of Church leadership from the period, contrasted with later official narrative framing; however, even complete documentary evidence cannot settle whether any private conviction of revelation was itself induced or genuine, which is a question the historical record cannot resolve.',
    'If historical evidence strongly supports a purely coercive account with no genuine internal revelatory conviction, this reading''s classification is reinforced and the endogenous reading''s claimed_type should show corresponding downward pressure toward snare-adjacent extraction; if strong evidence of independent revelatory conviction predating or exceeding the coercive pressure emerges, this reading becomes harder to sustain and the constraint family''s balance shifts toward the endogenous or hybrid reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_versus_coercion_causal_priority, conceptual, 'Whether historical evidence can or does adjudicate revelation versus coercion as the operative cause of the Manifesto.').

omega_variable(
    committer_framing_selection_bias,
    'Given that this story was generated as the exogenous_override reading specifically (rather than selecting a single best reading of ''the Manifesto''), does the selection of which reading to author first (or which reading a given generation run emphasizes) itself introduce a framing artifact into the corpus?',
    'Compare the three sibling readings'' authored epsilon values and beneficiary/victim structures for internal consistency; check whether the corpus as a whole treats the three readings with roughly comparable narrative development and evidentiary grounding, or whether one reading is authored with systematically richer stakeholder detail than the others.',
    'If the corpus systematically over-develops one reading relative to its siblings, downstream aggregate analysis of the marriage_commitment_legitimacy kernel would be biased toward that reading''s classification even though the kernel is genuinely contested among the three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_selection_bias, conceptual, 'Whether authoring order or relative narrative development among sibling kernel readings introduces corpus-level framing bias.').

omega_variable(
    plural_wives_agency_erasure,
    'To what extent does treating plural wives and children primarily as powerless victims of the coercive episode erase their own contested internal views — some publicly defended the practice as a source of religious and economic agency, others sought exit from it independent of federal pressure?',
    'Primary-source review of plural wives'' own writings, petitions, and testimony from the period (several publicly petitioned Congress in defense of the practice), disaggregated from the aggregate ''victim'' framing used in this constraint''s stakeholder surface.',
    'If a substantial number of plural wives held and acted on views inconsistent with the powerless/trapped characterization, the stakeholder situation for plural_wives_and_children should be revised toward greater heterogeneity, potentially splitting into separate stakeholder entries with different power/exit profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plural_wives_agency_erasure, empirical, 'Whether the powerless-victim characterization of plural wives homogenizes a genuinely heterogeneous set of documented positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1862, 0.2).
narrative_ontology:measurement(marr_tr_t1874, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1874, 0.28).
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1882, 0.4).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1887, 0.52).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.58).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1896, 0.63).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1904, 0.62).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1862, 0.35).
narrative_ontology:measurement(marr_be_t1874, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1874, 0.48).
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1882, 0.61).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1887, 0.74).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.79).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1896, 0.7).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1904, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement(marr_su_t1874, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1874, 0.45).
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1882, 0.68).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1887, 0.88).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.81).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1896, 0.55).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1904, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the marriage_commitment_legitimacy kernel, each authored as a separate file with its own epsilon per the epsilon-invariance principle. exogenous_override_reading treats federal coercion as the operative causal mechanism and classifies as tangled_rope with high extraction and a clear victim set (LDS membership, plural families) against a clear beneficiary (federal government, anti-polygamy coalition). endogenous_reinterpretation_reading treats prophetic revelation as the operative mechanism and is expected to classify closer to rope or mountain with negligible extraction. hybrid_pragmatic_reading treats deliberate strategic ambiguity as the operative mechanism and is expected to classify as scaffold, with a declared sunset condition around eventual doctrinal clarification. All three link to each other via affects_constraints; none supersedes the others — they represent genuinely contested framings held by different historical and contemporary parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
