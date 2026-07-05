% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath under Ecclesiastical Mediation (Church-Bounded Reading)
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the feudal oath kernel: the
 *   ecclesiastical mediation reading, in which the sacramental character of
 *   the feudal oath — sworn on relics, bound by Christian charity doctrine,
 *   enforceable through confession and threat of excommunication — genuinely
 *   constrains what a lord may extract from vassals, while simultaneously
 *   expanding the church's own interpretive jurisdiction and revenue base.
 *   This is a moderate-epsilon tangled rope: real coordination function (a
 *   ceiling on arbitrary lordly demands that vassals could invoke) coexisting
 *   with asymmetric extraction (the church's growing jurisdictional and
 *   financial stake in being the arbiter, and the fact that the ceiling
 *   protects vassals far more than it protects the peasant tenants who
 *   ultimately fund both lord and church). This is not the same constraint as
 *   the lord_extraction_reading (which denies any real ceiling exists) or the
 *   vassal_coordination_reading (which treats the obligations as fixed and
 *   charter-bound rather than doctrinally elastic and clergy-interpreted) —
 *   those are separate constraint files with their own epsilon values, linked
 *   here only by the shared kernel.
 *
 * KEY AGENTS:
 *   - ecclesiastical_authorities: Primary beneficiary and agenda-setter (institutional/arbitrage) — expands jurisdiction and revenue through moral mediation
 *   - constrained_lords: Primary payer of the moral ceiling (powerful/constrained) — extraction capacity checked by sacramental threat
 *   - vassal_smallholders: Secondary beneficiary (moderate/constrained) — gains recourse against lordly overreach
 *   - peasant_tenants: Excluded from the ceiling's protection, doubly taxed (powerless/trapped) — bears lord and church claims alike
 *   - canon_lawyers: Analytical/doctrinal observer — produces the interpretive apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.48).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath under Ecclesiastical Mediation (Church-Bounded Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '9c0382a9-ab09-41ac-80cf-7519e45f67cb').
narrative_ontology:cs_kernel_codification('9c0382a9-ab09-41ac-80cf-7519e45f67cb', distributed).
narrative_ontology:cs_authority_grounding('9c0382a9-ab09-41ac-80cf-7519e45f67cb', lineage).
narrative_ontology:cs_interpretation_layer_present('9c0382a9-ab09-41ac-80cf-7519e45f67cb').
narrative_ontology:cs_reading_relation('9c0382a9-ab09-41ac-80cf-7519e45f67cb', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c0382a9-ab09-41ac-80cf-7519e45f67cb', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('9c0382a9-ab09-41ac-80cf-7519e45f67cb', foundational, sacramental_oath_binds_conscience_beyond_secular_enforcement).
narrative_ontology:cs_axiom_status(sacramental_oath_binds_conscience_beyond_secular_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('9c0382a9-ab09-41ac-80cf-7519e45f67cb', sacramental_oath_binds_conscience_beyond_secular_enforcement, theological).
narrative_ontology:cs_axiom('9c0382a9-ab09-41ac-80cf-7519e45f67cb', foundational, church_holds_interpretive_authority_over_charitable_extraction_limits).
narrative_ontology:cs_axiom_status(church_holds_interpretive_authority_over_charitable_extraction_limits, holdable).
narrative_ontology:cs_axiom_grounding('9c0382a9-ab09-41ac-80cf-7519e45f67cb', church_holds_interpretive_authority_over_charitable_extraction_limits, conventional).
narrative_ontology:cs_reference_frame('9c0382a9-ab09-41ac-80cf-7519e45f67cb', gregorian_reform_ecclesiastical_supremacy).
narrative_ontology:cs_drift_state('9c0382a9-ab09-41ac-80cf-7519e45f67cb', rise_of_royal_common_law_courts, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c0382a9-ab09-41ac-80cf-7519e45f67cb', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_smallholders).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasant_tenants).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, constrained_lords).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, confessors, and canon lawyers interpret the oath's sacramental character, threatening excommunication or interdict against lords who extract beyond what charity and just-price doctrine permit. They adjudicate disputes over what counts as excessive exaction, collect tithes and donations flowing from moral leverage, and expand their own jurisdiction each time they mediate a secular dispute. Their exit from the arrangement is effectively arbitrage — they can escalate to Rome, withhold sacraments, or quietly decline to intervene, shaping outcomes without bearing direct material cost.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities, beneficiary).

% Hold military and juridical power over their vassals and tenants but must operate within a moral ceiling on extraction enforced through confession, public shaming, and threat of spiritual sanction. They cannot easily repudiate the oath's sacramental framing without undermining their own legitimacy, which itself rests partly on being seen as a Christian lord. Their appeals against ecclesiastical intervention go through the same church courts that constrain them.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, constrained_lords, payer,
    powerful, generational, constrained, regional).

% Minor knights and free tenants who hold land in exchange for service benefit from the ceiling ecclesiastical doctrine places on what a lord may demand, since it gives them recourse to church courts or public opinion when a lord's demands exceed customary and moral bounds. They cannot leave the tenurial relationship easily, but the church's mediating presence gives them a lever they would not otherwise have.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_smallholders, beneficiary,
    moderate, biographical, constrained, local).

% Unfree or semi-free cultivators who owe labor, rents, and dues to the lord and, functionally, tithes and fees to the church as well. The theological ceiling on lordly extraction is set with reference to what a lord of the manor and his knightly vassals owe each other; it does not systematically protect peasants, who continue to bear the base agricultural surplus extraction plus church tithes layered on top. Their exit is essentially none — flight risks legal reprisal and loss of any customary tenancy right they hold.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasant_tenants, payer,
    powerless, biographical, trapped, local).

% Royal and high-noble power operates partly outside this specific dyadic oath structure, negotiating directly with the church over investiture and jurisdiction. They are not centrally implicated in the vassal-lord moral ceiling this constraint describes, though they benefit or suffer from the wider church-crown balance of power it feeds into. Their voice on the specific question of lord-vassal extraction limits is largely absent from this constraint's operation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, kings_and_great_magnates, excluded,
    powerful, generational, mobile, national).

% Compile and interpret the doctrinal basis for what charity and sacramental obligation require of a lord, producing the texts and precedents that ecclesiastical authorities invoke. They shape the constraint's operation through scholarship and disputation rather than direct enforcement.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canon_lawyers, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__ecclesiastical_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared moral vocabulary and an external adjudicating body (the church) that lords and vassals both recognize, which genuinely dampens arbitrary escalation of demands within the lord-vassal relationship and gives smaller vassals a recourse against overreach that pure secular power would not offer.
% TRANSFER_FUNCTION: Moves interpretive and moral authority (and associated tithes, donations, and legal fees) to the church; moves a partial ceiling on extraction from lords to vassal smallholders; does not meaningfully redirect the underlying flow of agricultural surplus from peasant tenants, who pay both lord and church.
% ABSENT_VOICES: Peasant tenants, whose labor underwrites both lordly and ecclesiastical claims, have no direct standing in the doctrinal disputes that define the extraction ceiling — the ceiling is negotiated between lords, vassals, and clergy over what lords owe vassals, not what the manor owes its cultivators. Kings and great magnates operate the wider church-crown contest largely outside this particular dyadic constraint.
% DISAPPEARANCE_RATIONALE: If the sacramental and charitable framing of the oath vanished, lords would lose the specific moral leverage vassals and the church use against them; church courts would lose a major source of jurisdiction and revenue; vassal smallholders would lose a check on lordly demands; extraction from vassals would likely rise toward whatever lords could enforce by raw power alone. Peasant burdens, already outside the ceiling's direct protection, would change comparatively little.
% FOUNDING_PROBLEM: Early medieval lordship risked unbounded, arbitrary extraction from vassals whenever a lord's power exceeded custom or restraint; the church offered a doctrinal and institutional mechanism — sacramental oath-breaking as mortal sin, subject to confession and penance — to impose an external ceiling that neither secular custom alone nor vassal military capacity could reliably enforce.
% FOUNDING_PROBLEM_CORROBORATION: Vassal smallholders and canon lawyers attest the ceiling remains operative through church court records and confessional manuals into the later medieval period. Constrained lords and secular chroniclers increasingly describe the ecclesiastical ceiling as a jurisdictional foothold used to expand church authority and revenue rather than a genuine restraint, especially once royal courts began offering competing secular remedies for the same disputes — this is corroboration from outside the church's own self-description, though it comes from a rival power center (the crown) with its own extraction interest.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at moderate level (0.48) reflecting a genuine but partial ceiling: the doctrine measurably restrains lord-vassal extraction while the church's own claims (tithes, fees, jurisdictional fees) add a second extraction layer that grows over the interval as canon law elaborates. Suppression (0.55) reflects the coercive backing of excommunication and interdict, real but softer than physical coercion — it works through reputational and spiritual sanction, not direct force, hence moderate rather than extreme. Theater ratio rises over the four centuries modeled (0.12 to 0.32) as the doctrinal apparatus for adjudicating 'just extraction' becomes increasingly formulaic and revenue-oriented relative to its original moral-restraint function — an early Goodhart-style drift where the mediating machinery increasingly serves its own maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities sit near the beneficiary end: they gain jurisdiction, revenue, and moral authority from mediating the oath, with arbitrage-level exit (they can escalate, withhold sacraments, or step back without direct material loss). Constrained lords sit toward the target end: their extraction capacity is capped, and their exit from the theological framing is constrained by dependence on that same framing for their own legitimacy. Vassal smallholders benefit moderately — real recourse, but their exit from tenurial dependency remains constrained. Peasant tenants are declared victims because the ceiling this reading describes was negotiated with reference to lord-vassal obligations, not lord-peasant ones; they are trapped and bear extraction from both institutions without the doctrinal protection vassals enjoy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unbounded lordly extraction absent an external check — was genuinely live in the early medieval period when secular courts were weak and lordly power was close to arbitrary. Over the four-century interval, secular royal courts increasingly offered competing remedies, which is why founding_problem_status is authored as contested rather than flatly dead: the church's mediating function did not disappear, but its exclusivity and necessity eroded as alternative dispute-resolution mechanisms matured, even as the church's own institutional stake in the arrangement (revenue, jurisdiction) persisted or grew. Classifying this as tangled_rope rather than snare prevents mislabeling a structure with real, vassal-benefiting coordination function as pure extraction; classifying it as tangled_rope rather than rope prevents ignoring the asymmetric extraction the church itself captures and the peasant tenants who fall outside the ceiling's protection entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_ceiling_vs_jurisdictional_capture,
    'Did ecclesiastical mediation of the feudal oath function as a genuine, historically effective restraint on lordly extraction, or was the ''charity ceiling'' primarily a doctrinal cover the church used to expand its own jurisdiction and revenue over secular disputes?',
    'Comparative analysis of documented excommunication/interdict cases specifically invoked against lords for excessive extraction versus cases invoked for jurisdictional disputes unrelated to extraction; tracking whether vassal petitions to church courts produced material changes in lordly behavior over time.',
    'If predominantly genuine restraint, the coordination function dominates and the constraint sits closer to rope; if predominantly jurisdictional capture, extraction dominates and the constraint sits closer to snare or tangled_rope with a higher epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_ceiling_vs_jurisdictional_capture, empirical, 'Whether the ecclesiastical ceiling was substantive restraint or jurisdictional pretext.').

omega_variable(
    kernel_reading_selection,
    'Which of the three declared readings of the feudal oath kernel (ecclesiastical mediation, lord extraction, vassal coordination) best captures the operative structure in a given region and period — and is the choice itself contested by the historical actors, or only by modern historians?',
    'Regional and period-specific comparison: examine whether contemporaries (lords, vassals, clergy) themselves disputed which framing governed a specific oath dispute, versus modern historiographical disagreement about how to characterize the institution retrospectively.',
    'If contemporaries themselves treated the readings as live alternatives being fought over (e.g., a lord arguing the oath authorizes maximal extraction while a bishop argues it is charity-bound), this supports authoring genuinely distinct constraint files per reading, each active in different disputes. If the dispute is purely a modern historiographical artifact, the three readings may better be treated as observer-relative framings of one underlying practice rather than as ontologically distinct constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether the three kernel readings were contested in period or are a modern analytic decomposition.').

omega_variable(
    peasant_exclusion_from_ceiling,
    'Was the exclusion of peasant tenants from the doctrinal extraction ceiling a deliberate doctrinal boundary (charity obligations construed narrowly as owed only to free vassals) or an unaddressed gap that canon lawyers simply never extended downward?',
    'Textual analysis of canon law commentaries and confessional manuals for explicit statements about the scope of a lord''s charitable obligation toward unfree tenants versus enfeoffed vassals.',
    'A deliberate boundary would sharpen the victim classification of peasant_tenants as structurally excluded by design; an unaddressed gap would suggest the constraint''s victim profile arose from doctrinal underdevelopment rather than intent, which bears on how the constraint''s extraction is best characterized over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_exclusion_from_ceiling, empirical, 'Whether peasant exclusion from the ceiling was doctrinally deliberate or an unexamined gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement(feud_tr_t160, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 160, 0.22).
narrative_ontology:measurement(feud_tr_t240, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 240, 0.27).
narrative_ontology:measurement(feud_tr_t320, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 320, 0.3).
narrative_ontology:measurement(feud_tr_t400, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 400, 0.32).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(feud_be_t160, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 160, 0.44).
narrative_ontology:measurement(feud_be_t240, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 240, 0.46).
narrative_ontology:measurement(feud_be_t320, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 320, 0.47).
narrative_ontology:measurement(feud_be_t400, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 400, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(feud_su_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(feud_su_t160, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 160, 0.46).
narrative_ontology:measurement(feud_su_t240, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 240, 0.5).
narrative_ontology:measurement(feud_su_t320, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 320, 0.53).
narrative_ontology:measurement(feud_su_t400, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 400, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the feudal_oath_reciprocity kernel, each authored as a separate file per the ε-invariance principle. ecclesiastical_mediation_reading (this file, tangled_rope, moderate ε) models the church as gaining real interpretive authority while genuinely constraining lords; lord_extraction_reading models the oath as authorizing extraction bounded only by vassal capacity (higher ε, closer to snare); vassal_coordination_reading models the obligations as fixed and charter-enforced rather than clerically elastic (lower ε, closer to rope). All three are linked bidirectionally via affects_constraints because a shift in which reading dominates in a given region/period structurally redistributes legitimacy and enforcement capacity among the same set of underlying actors (lords, vassals, church, peasants).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
