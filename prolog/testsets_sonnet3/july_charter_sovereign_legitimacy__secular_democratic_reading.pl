% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter — Secular Democratic Reading of Sovereign Legitimacy
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   This constraint is one reading of the contested July Charter kernel — the
 *   founding document negotiated after a mass uprising against an
 *   authoritarian regime. Under this reading, the charter mandates secular
 *   democratic institutions and subordinates the military to civilian
 *   authority. This is not the only defensible reading: a guided-nationalism
 *   reading treats the charter as establishing Islamic-nationalist
 *   legitimacy, and a military-custodian reading treats it as ratifying the
 *   armed forces as permanent institutional guardian. Each reading is a
 *   structurally distinct constraint with its own beneficiary/victim set and
 *   its own extraction profile — this file authors only the
 *   secular-democratic reading, cleanly, without averaging across the
 *   contest. Under this reading, Jamaat-e-Islami's foundational claim
 *   (religious identity as sovereign ground) and the military's claim to
 *   autonomous guardianship are both structurally excluded or constrained,
 *   making them the reading's victim set.
 *
 * KEY AGENTS:
 *   - secular_civil_society_coalitions: primary agenda-setter, drafted and campaigns for the secular-democratic framing (organized/constrained)
 *   - reformist_political_parties: primary beneficiary, gains electoral pathway under secular rules (organized/constrained)
 *   - jamaat_e_islami: primary target, foundational ideological claim excluded or constrained (organized/constrained)
 *   - military_autonomous_authority: primary target, institutional autonomy subordinated to civilian oversight (institutional/constrained)
 *   - civilian_oversight_bodies: co-agenda-setter, newly empowered but institutionally untested (institutional/constrained)
 *   - international_donors_and_observers: analytical observer, monitors implementation as aid/legitimacy condition (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.58).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.62).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter — Secular Democratic Reading of Sovereign Legitimacy").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '5b5dce9c-9bac-47be-84c8-0741b56f2e1e').
narrative_ontology:cs_kernel_codification('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', formalized).
narrative_ontology:cs_authority_grounding('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', lineage).
narrative_ontology:cs_interpretation_layer_present('5b5dce9c-9bac-47be-84c8-0741b56f2e1e').
narrative_ontology:cs_reading_relation('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', foundational, sovereignty_grounded_in_civic_secular_consent).
narrative_ontology:cs_axiom_status(sovereignty_grounded_in_civic_secular_consent, holdable).
narrative_ontology:cs_axiom_grounding('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', sovereignty_grounded_in_civic_secular_consent, conventional).
narrative_ontology:cs_axiom('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', foundational, military_authority_strictly_derivative_of_civilian_mandate).
narrative_ontology:cs_axiom_status(military_authority_strictly_derivative_of_civilian_mandate, holdable).
narrative_ontology:cs_axiom_grounding('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', military_authority_strictly_derivative_of_civilian_mandate, conventional).
narrative_ontology:cs_reference_frame('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', post_uprising_secular_constitutional_settlement).
narrative_ontology:cs_drift_state('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', contemporary_implementation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b5dce9c-9bac-47be-84c8-0741b56f2e1e', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civil_society_coalitions).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, reformist_political_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_professional_middle_class).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and campaigned for the charter's secular-democratic clauses, drawing on the post-uprising legitimacy of the movement that toppled the prior regime. They administer the interpretive framing of the charter through public advocacy, legal challenges, and alliances with reformist parties. Their continued relevance depends on the charter's secular provisions holding against both Islamist and military counter-claims.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civil_society_coalitions, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civil_society_coalitions, beneficiary).

% Gain a constitutional pathway to electoral competition on secular-democratic terms, insulated in principle from both religious-party dominance and military tutelage. They benefit from the charter's civilian-supremacy clauses but must actively contest interpretation with Islamist blocs and hedge against a military that retains informal leverage.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, reformist_political_parties, beneficiary,
    organized, biographical, constrained, national).

% Benefits from a secular constitutional order that protects pluralism, minority rights, and a predictable legal environment for commerce and civil life. Largely urban and internationally networked, this group has more exit optionality (emigration, capital mobility) than rural or lower-income constituencies who live more directly under whichever reading of the charter prevails locally.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_professional_middle_class, beneficiary,
    moderate, biographical, mobile, national).

% A long-established religious-political organization whose preferred framework — religious identity as the ground of sovereign legitimacy — is structurally excluded or heavily constrained under this reading of the charter. Faces legal restriction, exclusion from coalition politics, and delegitimization of its founding ideological claim. Its exit options are limited to underground organizing, electoral participation under constraint, or protracted legal/political contestation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, generational, constrained, national).

% The armed forces' institutional claim to autonomous guardianship over national stability — historically a source of leverage, coup capacity, and budgetary independence — is subordinated to civilian oversight under this reading. The military retains substantial informal power (patronage networks, economic holdings, security-sector influence) but loses the constitutional cover for direct political intervention. Its exit option is noncompliance or slow-walking implementation rather than open confrontation, given the charter's post-uprising legitimacy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, generational, constrained, national).

% Rural and religiously conservative populations whose preferred vision of religiously-grounded governance is marginalized by the secular framework. They lack the organizational capacity of Jamaat-e-Islami or the mobility of urban elites; the charter's secular provisions are experienced as an imposed framework rather than a negotiated settlement, with little practical recourse beyond electoral voice, which is itself constrained by the new order's rules.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_constituencies, payer,
    powerless, biographical, trapped, regional).

% Parliamentary defense committees, civilian ministries of defense, and judicial review bodies newly empowered (on paper) to oversee military conduct, budgets, and command appointments. Their actual capacity to enforce subordination depends on political will, institutional maturity, and whether the military's informal power networks are dismantled or merely redirected.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_oversight_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Foreign governments, multilateral lenders, and human rights organizations monitor implementation of the secular-democratic provisions as a condition of aid, trade preferences, or diplomatic recognition. They can amplify or withhold legitimacy for this reading of the charter but do not directly administer it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_donors_and_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, enforceable rule set for post-uprising governance: elections, civilian control of the security apparatus, and secular legal equality, intended to prevent both a relapse into military rule and a slide into a religiously-defined state — solving the genuine coordination problem of how a fractured post-revolutionary polity agrees on what counts as legitimate authority going forward.
% TRANSFER_FUNCTION: Moves interpretive and institutional legitimacy away from religious-political claims (Jamaat-e-Islami's framework) and away from military self-authorization, toward secular civilian institutions and the coalitions that drafted the charter's language — reallocating political standing, legal protection, and access to state power along secular-democratic lines.
% ABSENT_VOICES: Jamaat-e-Islami and allied religious-political actors, along with rural religious-conservative constituencies, would object that their preferred vision of sovereignty grounded in religious identity is being written out of the founding document; they are present in the broader national conversation but structurally excluded from shaping this specific reading's interpretive authority.
% DISAPPEARANCE_RATIONALE: If the secular-democratic reading collapsed as the operative interpretation of the charter, the interpretive vacuum would almost certainly be filled by one of its sibling readings (guided nationalism or military custodianship), reallocating institutional power toward religious-political actors or back toward the military — civilian oversight bodies would lose their charter-based mandate, and reformist parties would face a fundamentally different playing field.
% FOUNDING_PROBLEM: In the aftermath of a mass uprising that toppled an authoritarian regime, the country needed a foundational settlement that could prevent recurrence of both unaccountable military rule and any single faction (secular or religious) monopolizing the new order — the charter was meant to encode a durable, pluralistic transition.
% FOUNDING_PROBLEM_CORROBORATION: Reformist parties and secular civil society coalitions attest the founding problem (preventing relapse into authoritarianism) remains live and this reading is its correct solution. Independent international observers and constitutional scholars outside the coalition corroborate that military subordination remains only partially implemented, while Jamaat-e-Islami and rural religious constituencies attest — from outside the beneficiary set — that the charter's secular framing itself constitutes a new exclusionary settlement rather than a neutral solution to the founding problem.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate-high 0.58 by interval end: the reading imposes real, contested costs on Jamaat-e-Islami and military autonomy — exclusion from constitutional legitimacy is a substantial transfer, not a neutral procedural rule. Suppression (0.62) reflects the active legal and political machinery required to hold the exclusion in place — election-law restrictions on religious parties, oversight legislation constraining military budgets and appointments — none of which is self-enforcing. Theater ratio (0.40) captures a real but partially performative dimension: oversight bodies exist on paper with more institutional theater than enforcement capacity in the early years, though this narrows somewhat as implementation matures. All three series share one time grid (0, 4, 8, 12, 16, 20, 24) so no metric is back-filled from an end-state value.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular civil society coalitions and reformist parties sit near the beneficiary end: they authored the framework and gain institutional standing from it, with organized power but genuinely constrained exit (they cannot simply leave the polity they are building). Jamaat-e-Islami and military autonomous authority sit near the target end: their foundational claims are structurally excluded, they bear the transfer, and their exit options are constrained rather than mobile — the military in particular retains informal leverage but loses constitutional cover, which functions as extraction of political standing even without a direct financial transfer. Religious conservative constituencies are the most vulnerable payer: powerless, trapped, and without the organizational capacity Jamaat-e-Islami has to contest the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing relapse into either military rule or religious-factional monopoly) remains genuinely contested rather than settled — this is not a case of a mandate persisting after its problem died. The secular-democratic reading's proponents assert the problem is fully live and their solution is correct; the excluded parties assert the 'solution' is itself a new exclusionary settlement. Because corroboration comes from both inside and outside the beneficiary set with disagreement intact, this is authored as tangled_rope rather than snare: there is a genuine coordination function (preventing relapse into authoritarianism serves a real collective interest across factions) bundled with asymmetric extraction (the specific secular framing transfers legitimacy away from identifiable losing parties) — both must be true simultaneously for the classification to hold, and the schema's requirement of active enforcement, at least one beneficiary, and at least one payer is satisfied structurally, not by metric-tuning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the interpretive dispute over the July Charter''s text locate itself — is it a genuine textual ambiguity (the charter''s language underdetermines which reading is correct), or is the text reasonably clear and the contest is purely about enforcement and political power?',
    'Close textual/drafting-history analysis of the charter''s actual clauses on religion, military command authority, and civilian oversight, cross-referenced against the negotiating record and drafters'' contemporaneous statements.',
    'If the text is genuinely ambiguous, all three readings (secular-democratic, guided-nationalism, military-custodian) have comparable textual footing and the contest is a live constitutional dispute. If the text clearly mandates secular democratic control, the sibling readings are better understood as attempts to overwrite a settled text through political pressure rather than competing legitimate interpretations — this would sharpen the suppression metric for this reading (defending a clear text against revision) versus the sibling readings (which would then look more like snares).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the three kernel readings reflect genuine textual ambiguity or differing political projects layered onto a reasonably determinate text.').

omega_variable(
    military_subordination_depth,
    'Does civilian oversight of the military under this reading represent genuine subordination of the military''s operational and budgetary autonomy, or a formal/legal subordination that leaves informal power networks (patronage, economic holdings, security dominance) substantially intact?',
    'Track military budget transparency, civilian veto exercise over command appointments, and any instances of military noncompliance with civilian directives over a multi-year implementation window.',
    'If subordination is substantially formal only, the effective extraction borne by ''military_autonomous_authority'' as a victim group is overstated, and the sibling military_custodian_reading is empirically closer to the operative reality even if this reading is legally correct — this would suggest the secular-democratic reading''s classification, while structurally accurate to the charter''s text, understates how much extraction the military reading is actually still achieving informally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_subordination_depth, empirical, 'Whether formal military subordination under this reading translates into substantive loss of military autonomy.').

omega_variable(
    religious_exclusion_proportionality,
    'Is the constraint on Jamaat-e-Islami and religious-political organizing proportionate to a genuine coordination need (preventing a return to sectarian violence or theocratic capture), or does it extend into viewpoint suppression that exceeds what secular-democratic coordination requires?',
    'Comparative analysis of the specific legal restrictions placed on religious parties against restrictions in other post-authoritarian secular transitions, and assessment of whether narrower rules (e.g., barring only violence-linked factions) would achieve the same coordination goal.',
    'If the exclusion is broader than proportionate, the tangled_rope classification''s coordination component is weaker than authored and the constraint drifts closer to snare with respect to the religious-conservative victim set specifically, even while retaining a genuine coordination function with respect to the military.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_exclusion_proportionality, preference, 'Whether the scope of political-Islam exclusion is calibrated to a genuine coordination need or exceeds it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(july_be_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(july_su_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the july_charter_sovereign_legitimacy kernel, each authored as a separate constraint story with its own ε, beneficiary/victim set, and classification per the ε-invariance principle. The secular_democratic_reading authors a tangled_rope (genuine transitional coordination function bundled with exclusion of political-Islam actors and military autonomy as victims). The guided_nationalism_reading and military_custodian_reading are expected to author substantially different beneficiary/victim structures and likely different classifications, reflecting the genuinely contested nature of the founding text's interpretive authority. All three link to each other via affects_constraints because a shift in which reading holds operative legitimacy directly reallocates the others' beneficiary/victim standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
