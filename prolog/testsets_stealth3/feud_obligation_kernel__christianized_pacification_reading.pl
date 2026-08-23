% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification Regime over Blood-Feud Obligations (Divine-Law Reading)
 *   domain: legal_anthropology/medieval_history/political_theology
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the feud_obligation_kernel: the
 *   christianized_pacification_reading, under which the standing arrangement
 *   under contest is the ecclesiastical-royal prohibition regime built
 *   against private vengeance from the Peace of God councils (989) through
 *   the Fourth Lateran Council's consolidation (1215) and its aftermath (to
 *   1250). Epsilon's referent is that standing pacification arrangement
 *   itself, the ban on vengeance plus the delegated-violence doctrine plus
 *   the penitential machinery, assessed by this reading's own lights; it is
 *   never the feud system the reading condemns and never the arrangement the
 *   reading would endorse instead. The claim and the metrics are independent
 *   authored facts: the claim is tangled_rope, a genuine coordination
 *   function (bounding unbounded vendetta cycles) fused with asymmetric
 *   extraction (jurisdictional monopoly, composition flows, and a licensing
 *   rent on legitimate violence), while the metrics describe substantially
 *   extractive, actively enforced operation whose enforcement intensity rose
 *   across the interval. Sibling readings are other constraints in other
 *   files, linked here through the network block and not averaged.
 *   Assumptions stated: provenance commits mirror the governing revisions of
 *   the one-shot example; sampling parameters assume the corpus default
 *   temperature. KEY AGENTS (by structural relationship): church_hierarchy:
 *   agenda-setter and principal collector (institutional/arbitrage), authors
 *   the ban, runs penitential enforcement, accrues jurisdiction and
 *   composition flows; royal_justice_administrations: secondary beneficiary
 *   turned co-administrator (institutional/arbitrage), inherits the monopoly
 *   as royal peace; feud_obligated_kindreds: primary target
 *   (organized/identity_locked), bear the ban as spiritual peril, penitential
 *   coercion, and loss of customary justice; peasant_communities: protected
 *   constituency carrying diffuse costs (powerless/trapped);
 *   customary_law_keepers: excluded voice (moderate/constrained), displaced
 *   legitimators of feud law; comparative_feud_anthropologists: analytical
 *   observer (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.63).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.78).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification Regime over Blood-Feud Obligations (Divine-Law Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/political_theology").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '1fb61eb5-f37a-4d0f-bdab-dc847df58151').
narrative_ontology:cs_kernel_codification('1fb61eb5-f37a-4d0f-bdab-dc847df58151', fixed_text).
narrative_ontology:cs_authority_grounding('1fb61eb5-f37a-4d0f-bdab-dc847df58151', lineage).
narrative_ontology:cs_interpretation_layer_present('1fb61eb5-f37a-4d0f-bdab-dc847df58151').
narrative_ontology:cs_reading_relation('1fb61eb5-f37a-4d0f-bdab-dc847df58151', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('1fb61eb5-f37a-4d0f-bdab-dc847df58151', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('1fb61eb5-f37a-4d0f-bdab-dc847df58151', foundational, vengeance_divine_prerogative_only).
narrative_ontology:cs_axiom_status(vengeance_divine_prerogative_only, holdable).
narrative_ontology:cs_axiom_grounding('1fb61eb5-f37a-4d0f-bdab-dc847df58151', vengeance_divine_prerogative_only, theological).
narrative_ontology:cs_axiom('1fb61eb5-f37a-4d0f-bdab-dc847df58151', foundational, legitimate_violence_delegated_to_sacred_institutions).
narrative_ontology:cs_axiom_status(legitimate_violence_delegated_to_sacred_institutions, holdable).
narrative_ontology:cs_axiom_grounding('1fb61eb5-f37a-4d0f-bdab-dc847df58151', legitimate_violence_delegated_to_sacred_institutions, theological).
narrative_ontology:cs_reference_frame('1fb61eb5-f37a-4d0f-bdab-dc847df58151', divine_violence_monopoly).
narrative_ontology:cs_drift_state('1fb61eb5-f37a-4d0f-bdab-dc847df58151', post_lateran_fourth, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('1fb61eb5-f37a-4d0f-bdab-dc847df58151', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_justice_administrations).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, peasant_communities).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_obligated_kindreds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, peasant_communities).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, divine_vengeance_reservation_doctrine).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, delegated_violence_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares through councils, decretals, and the pulpit that vengeance belongs to God alone and that private retaliation is mortal sin; administers the penitential machinery that operationalizes the ban through confession, public penance, pilgrimage sentences, excommunication, and interdict. Receives composition payments and mediation gifts, acquires land in feud settlements it brokers, consecrates or withholds the legitimacy of armed enterprises, and extends its courts into matters previously governed by kin law. Its enforcement arm is spiritual sanction backed by social exclusion; its position in the arrangement is effectively unexited, since it authored the arrangement and can redefine it.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy, beneficiary).

% Adopts the divine-law ban on private vengeance as charter and precedent for royal peace (the king's peace, the Landfrieden), converting feud cases into crown cases punishable by fines and forfeiture. Builds itinerant justices, court staff, and record-keeping on the back of the prohibition, and collects amercements from its breach. Early in the interval it borrows ecclesiastical legitimacy for its peace; by the interval's end it asserts peace-keeping by its own authority, reducing the Church toward a junior partner in deciding which violence is lawful.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_justice_administrations, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_justice_administrations, agenda_setter).

% Kin corporations bound to answer a slain kinsman with vengeance or negotiated composition. The vengeance duty is constitutive of kindred standing: a house that declines to pursue its dead loses marriage alliances, retainers, and voice in local assembly, so declining the duty is social death rather than a priced cost. Under the prohibition the same duty now exposes them to excommunication, compulsory penance, and settlement on terms mediated by the Church; their customary justice is relabeled as sin while the underlying grievance remains addressed by no mechanism they control. Obligations transmit across generations, binding heirs to quarrels they did not begin.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_obligated_kindreds, payer,
    organized, generational, identity_locked, local).

% The named protegés of the peace oaths: sworn protection for plowmen, clergy, and church property against knightly raiding gives villages intervals of safety they could not otherwise purchase. They pay through tithes supporting the penitential apparatus, through composition levies passed down from kindred settlements, and, when licensed expeditions replace private feud, through provisions and levies for sanctioned war.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, peasant_communities, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, peasant_communities, payer).

% Lay arbiters, oath-helpers, and reciters of unwritten kin-law who legitimated feud and wergild before conciliar courts existed. They hold no seat in the synods and chanceries drafting the ban; illiteracy and the language barrier exclude them from the written record. Their objection, that the old law balanced grievances without spiritualizing them, survives chiefly in the persistence of feud practice and scattered lay testimony inside dispute records.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, customary_law_keepers, excluded,
    moderate, generational, constrained, local).

% Modern analysts comparing feud across stateless and state-forming societies, using segmentary lineages, saga-era Iceland, and customary codes as controls. They reconstruct what the pacification campaign changed by holding societies without such a campaign in view, and they participate in no period institution; their seat exists to see the whole structure at once.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, comparative_feud_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces decentralized retaliatory justice with centralized adjudication: converts open-ended vengeance cycles between kindreds into bounded composition and settlement processes under ecclesiastical mediation, and shields designated non-combatants through peace-oath protection.
% TRANSFER_FUNCTION: Moves jurisdictional authority, material compositions (settlement shares, penitential payments, gifts and lands conveyed to monasteries), and the right to license armed violence away from kin groups and toward ecclesiastical and, increasingly, royal institutions.
% ABSENT_VOICES: Customary law-keepers and feud-obligated kindreds had no literate seat in the conciliar deliberations that drafted the ban; the record of lay objection survives mostly in the persistence of feud itself and occasional royal pushback, not in authored dissent.
% DISAPPEARANCE_RATIONALE: If the prohibition regime vanished overnight, kin-based vengeance would resume as the default justice mechanism wherever royal writ ran thin; the Church would lose a major jurisdiction and its associated revenues; royal peace-keeping, built on the ban's precedents, would lose its doctrinal floor; and the European path toward a state monopoly on legitimate violence would reroute.
% FOUNDING_PROBLEM: Unbounded retaliatory violence between kindreds in the absence of effective royal enforcement: killings begot killings, non-combatants were exposed to raid and reprisal, and deterrence failed to stabilize the cycle.
% FOUNDING_PROBLEM_CORROBORATION: Royal administrative records and charters outside the Church corroborate the violence levels the founding problem describes, and comparative ethnography of feud in societies lacking centralized enforcement corroborates the underlying order-maintenance problem; the Church's own homiletic account of universal carnage is not the sole or controlling source. On status: royal administrators increasingly attest that the function has migrated to their courts, while ecclesiastical authorities attest the problem remains live as sin; the parties therefore dispute it.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.63 at interval end) reflects the widening gap between the regime's protective promise and its institutional yield: composition payments, mediation fees, land grants conveyed in brokered settlements, tithe support for the penitential apparatus, and decisively the jurisdictional rent of deciding which violence is legitimate. The campaign redirected knightly violence through crusade preaching and truce-bounded seasons rather than eliminating it, which is the signature of a licensing monopoly rather than simple pacification. Suppression (0.78) is authored as the raw structural intensity of the enforcement machinery, excommunication, interdict, public penance, and confiscation, aimed at complete suppression of feud practice; per the framework suppression is not scaled by power or scope, only extractiveness is scaled. Theater (0.30) is moderate-low: the rituals were functional, public penance restored peace and oath processions mobilized consensus, though a growing share of activity defended the jurisdiction itself. Accessibility collapse (0.55) is partial: wergild composition and church-mediated settlement remained workable alternatives and feud persisted for centuries where royal writ ran thin, so alternatives narrowed without vanishing. Resistance (0.60) is real and durable: magnates defied episcopal courts, individual bishops personally waged feud, and the obligation survived four centuries of legislation. The three tracked series run on ONE shared grid (989, 1030, 1070, 1110, 1150, 1200, 1250) with every metric authored at every point. Suppression_requirement is tracked as a series rather than left to the static scalar because the story's dynamic is enforcement-capacity maturation, from ad hoc conciliar oaths through Gratian's systematization to Lateran IV's mandatory annual confession, which converted the confessional into a detection instrument for feud intention; a flat-trajectory story would have omitted it. Coalition check: kindreds' theoretical coalition power is noted and discounted, since the same kin fragmentation that kept feud persistent prevented the coalition that could have bargained the ban down.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is divinely ordained order the Church is privileged to administer; from the payer seat it is dispossession, a lawful justice mechanism relabeled as mortal sin with the substitute controlled by the relabeler; the spiritual-peril threat binds the payer twice, once through the grievance and once through the conscience. The beneficiary seat experiences it as an inheritable fiscal and jurisdictional windfall arriving already ideologically justified. The excluded seat experiences the entire transaction as a jurisdictional coup conducted in a written latinate language its holders could not enter. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows. church_hierarchy sits at the beneficiary pole: it collects compositions, jurisdiction, and the licensing rent, and it authored the constraint it enforces, so its derived d sits near 0.0. royal_justice_administrations derives near the beneficiary end but slightly above the Church, since it bears enforcement costs it does not fully control. feud_obligated_kindreds derive near the full-target pole, and the identity lock pushes them further toward it: the vengeance duty is constitutive of kindred standing, refusal means social death, so exit is an identity rupture rather than a priced option. peasant_communities derive low d from named protection with a payer-side drag from tithes and levies. No directionality overrides were needed: the derivation chain from beneficiary and victim declarations plus exit options reproduces the structural relationships, and no seat exhibits the capture-style distortion overrides exist to correct. Scope effects apply to extractiveness only; the regime's continental reach makes verification harder and amplifies effective extraction modestly at the payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric mislabels. Read as pure snare, a Church power grab dressed in scripture, the account erases the coordination actually delivered: peace-oath protection measurably shielded noncombatants and composition regimes did bound vendettas where they took hold. Read as pure rope, grace displacing barbarism, it erases the extraction: the monopoly on legitimate violence was a rent, the licensing of crusading shows the ban was selective, and the jurisdiction expanded by condemning the very practices that fed it. Tangled_rope holds both halves together. On obsolescence: the founding problem, inter-kindred violence beyond state reach, was live at founding and contested at interval end, since royal adjudication was absorbing the function without extinguishing the underlying condition; founding_problem_status contested paired with disappearance_verdict world_rearranges records an arrangement whose mandate is migrating rather than dead, so the dead-mandate mismatch flag finds no footing here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermination,
    'Which reading of feud_obligation_kernel should classify the arrangement: this divine-law reading, the stateless-coordination reading, or the extraction-cycle reading?',
    'No empirical resolution exists, the choice is a framing commitment. Signals that guided this file: the declared structural delta assigns the Church the beneficiary seat and feud participants the victim seat, a structure only the christianized reading generates.',
    'Adopting the stateless-coordination sibling removes the Church from the beneficiary set entirely and redescribes kindreds as mutual coordinators, trending the type toward rope; adopting the extraction-cycle sibling makes productive classes the victims and the warrior elites the beneficiaries, trending toward snare with diffuse gain flow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Committer-frame ambiguity: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    spiritual_peril_victimhood_status,
    'Is the victimhood of feud participants under this reading constituted by genuinely operative spiritual peril, kin duty compelling mortal sin under penalty of dying unshriven, or by doctrinal construction layered over ordinary coercion?',
    'Examine penitential manuals, exempla, and testamentary behavior for whether contemporaries experienced the peril as operative fear rather than clerical rhetoric; cross-check with comparative evidence on internalized religious dread.',
    'If doctrinal construction, the victim set thins to material losses, jurisdiction, composition, and coerced settlement, and epsilon falls materially; if genuinely operative, the spiritual-peril victimization stands and the identity lock on kindreds deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_peril_victimhood_status, conceptual, 'Whether the declared victim set rests on doctrine-relative harm or on harm operative independent of the doctrine.').

omega_variable(
    delegation_boundary_erosion,
    'Does royal appropriation of peace-keeping, the king''s peace and the Landfrieden, remain inside the frame''s delegation clause or exceed it?',
    'Legal-historical analysis of royal charters and canonist commentary: trace whether royal violence claims derivation from divine and ecclesiastical delegation or asserts independent sacral authority.',
    'If royal authority exceeds delegation, the reading''s frame fractures at interval end; extraction attribution migrates from Church to emergent state and the drift vector steepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_boundary_erosion, empirical, 'Where legitimate delegation ends as royal institutions mature.').

omega_variable(
    licensed_violence_tension,
    'Does the regime''s licensing of mass sanctioned violence, crusade indulgences and truce-seasoned warfare, reveal the ban as a violence-market monopoly rather than pacification?',
    'Compare violence incidence and sanctioned-expedition participation against declared doctrine; test whether the monopoly price, indulgence, composition, or license, tracks restraint or mere redirection of violence.',
    'If licensing dominates, the coordination function narrows to market-making in legitimate violence and classification trends toward snare with the Church as capturer of a licensing rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensed_violence_tension, conceptual, 'Pacification versus selective violence-licensing monopoly.').

omega_variable(
    penitential_suppression_efficacy,
    'Did penitential discipline actually reduce feud violence where it operated, or did feud persist beneath compliant performance?',
    'Quantitative reconstruction of homicide and vendetta rates inside versus outside episcopal jurisdiction across the interval, comparing regions of intensive penitential enforcement against matched controls.',
    'If ineffective, the coordination half of the tangled_rope claim hollows and the arrangement trends toward piton or snare, performance without function; if effective, the rope content is confirmed and the coordination share of epsilon is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penitential_suppression_efficacy, empirical, 'Empirical efficacy of the suppression machinery against feud persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 989, 1250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_xpac_tr_t989, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 989, 0.18).
narrative_ontology:measurement_basis(feud_xpac_tr_t989, observed).
narrative_ontology:measurement(feud_xpac_tr_t1030, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1030, 0.2).
narrative_ontology:measurement_basis(feud_xpac_tr_t1030, observed).
narrative_ontology:measurement(feud_xpac_tr_t1070, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1070, 0.22).
narrative_ontology:measurement_basis(feud_xpac_tr_t1070, observed).
narrative_ontology:measurement(feud_xpac_tr_t1110, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1110, 0.25).
narrative_ontology:measurement_basis(feud_xpac_tr_t1110, observed).
narrative_ontology:measurement(feud_xpac_tr_t1150, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1150, 0.27).
narrative_ontology:measurement_basis(feud_xpac_tr_t1150, observed).
narrative_ontology:measurement(feud_xpac_tr_t1200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1200, 0.29).
narrative_ontology:measurement_basis(feud_xpac_tr_t1200, observed).
narrative_ontology:measurement(feud_xpac_tr_t1250, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1250, 0.3).
narrative_ontology:measurement_basis(feud_xpac_tr_t1250, observed).

% Extraction over time
narrative_ontology:measurement(feud_xpac_be_t989, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 989, 0.44).
narrative_ontology:measurement_basis(feud_xpac_be_t989, observed).
narrative_ontology:measurement(feud_xpac_be_t1030, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1030, 0.48).
narrative_ontology:measurement_basis(feud_xpac_be_t1030, observed).
narrative_ontology:measurement(feud_xpac_be_t1070, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1070, 0.52).
narrative_ontology:measurement_basis(feud_xpac_be_t1070, observed).
narrative_ontology:measurement(feud_xpac_be_t1110, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1110, 0.55).
narrative_ontology:measurement_basis(feud_xpac_be_t1110, observed).
narrative_ontology:measurement(feud_xpac_be_t1150, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1150, 0.58).
narrative_ontology:measurement_basis(feud_xpac_be_t1150, observed).
narrative_ontology:measurement(feud_xpac_be_t1200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1200, 0.61).
narrative_ontology:measurement_basis(feud_xpac_be_t1200, observed).
narrative_ontology:measurement(feud_xpac_be_t1250, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1250, 0.63).
narrative_ontology:measurement_basis(feud_xpac_be_t1250, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_xpac_su_t989, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 989, 0.42).
narrative_ontology:measurement_basis(feud_xpac_su_t989, observed).
narrative_ontology:measurement(feud_xpac_su_t1030, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1030, 0.52).
narrative_ontology:measurement_basis(feud_xpac_su_t1030, observed).
narrative_ontology:measurement(feud_xpac_su_t1070, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1070, 0.6).
narrative_ontology:measurement_basis(feud_xpac_su_t1070, observed).
narrative_ontology:measurement(feud_xpac_su_t1110, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1110, 0.66).
narrative_ontology:measurement_basis(feud_xpac_su_t1110, observed).
narrative_ontology:measurement(feud_xpac_su_t1150, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1150, 0.71).
narrative_ontology:measurement_basis(feud_xpac_su_t1150, observed).
narrative_ontology:measurement(feud_xpac_su_t1200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1200, 0.75).
narrative_ontology:measurement_basis(feud_xpac_su_t1200, observed).
narrative_ontology:measurement(feud_xpac_su_t1250, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1250, 0.78).
narrative_ontology:measurement_basis(feud_xpac_su_t1250, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of feud_obligation_kernel per the epsilon-invariance principle: the colloquial label blood-feud obligations conflates three structurally distinct claims. This file carries the christianized reading's instantiation, the prohibition-and-penitential regime with the Church as beneficiary and kindreds as victims, epsilon approximately 0.63. The stateless-coordination sibling carries feud-as-self-enforcing-justice with no Church beneficiary and epsilon near the coordination floor. The extraction-cycle sibling carries feud-as-productive-depletion with warrior elites as beneficiaries and productive classes as victims. Edges run by evidentiary dependence: this reading cites the violence levels the coordination sibling explains, and the extraction-cycle sibling quantifies the damage both others litigate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
