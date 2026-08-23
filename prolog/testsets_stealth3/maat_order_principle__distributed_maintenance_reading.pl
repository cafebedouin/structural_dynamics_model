% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at as Distributed Maintenance Obligation (Distributed-Maintenance Reading)
 *   domain: religious/political philosophy (ancient Egypt)
 *
 * SUMMARY:
 *   Ancient Egyptian normative order held that Ma'at — right order, justice,
 *   truth — is sustained not by the king alone but by every actor performing
 *   the conduct proper to their station: the king upholds cosmic order
 *   through ritual and just rule, officials through impartial judgment,
 *   priests through cult service, scribes through accurate administration,
 *   peasants through labor and payment, households through care and honesty.
 *   Authority in this arrangement tracks demonstrated maintenance: a judge's
 *   standing rests on judgments rendered, a steward's on stores kept, the
 *   king's on the flood arriving and justice holding — the First Intermediate
 *   Period stood as standing evidence that failure at the top was possible
 *   and consequential. This file instantiates the
 *   distributed_maintenance_reading of the maat_order_principle kernel as a
 *   single ε-invariant constraint: the standing distributed-maintenance
 *   arrangement assessed by this reading's own lights, with ε (0.38) authored
 *   for THAT arrangement only. FAMILY NOTE (ε decomposition): the colloquial
 *   label 'Ma'at' covers at least three structurally distinct constraints
 *   sharing one kernel — this reading (multiple legitimate interpreters,
 *   authority conditional on demonstrated maintenance, lowest extraction in
 *   the family via distributed accountability, ε≈0.38);
 *   maat_order_principle__divine_mandate_reading (order flows through an
 *   inherently-ordered apex who cannot violate it by definition;
 *   interpretation concentrated at the top, elite accountability dissolved,
 *   highest extraction, ε expected well above 0.6); and
 *   maat_order_principle__reciprocity_reading (mutual ruler-subject
 *   obligations; the apex bound contractually but as sole counterparty,
 *   intermediate extraction, ε≈0.5). Citation pressure runs from royal
 *   ideology toward the wisdom-literature generalization; each member links
 *   the others via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - pharaoh_bound_by_ma_at: administrator of the norm and
 *   itself bound by it (institutional/identity_locked) - temple_priesthood:
 *   principal collector of the arrangement's material flows
 *   (institutional/identity_locked) - scribal_administrative_class: operator
 *   of assessment and recording machinery (organized/constrained) -
 *   judicial_officialdom: standing-collecting adjudicators, themselves
 *   reviewable (institutional/constrained) - corvee_laboring_peasantry:
 *   heaviest material obligors, holders of petition and strike leverage
 *   (powerless/trapped) - foreign_war_captives: outside the ordered world's
 *   circle of obligation (powerless/trapped) - egyptological_analysts:
 *   analytical observer — sees the full structure including its boundary
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.38).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.46).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at as Distributed Maintenance Obligation (Distributed-Maintenance Reading)").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "religious/political philosophy (ancient Egypt)").

domain_priors:requires_active_enforcement(maat_order_principle__distributed_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '74391d9a-125a-47f1-97f1-b79a1c5ac760').
narrative_ontology:cs_kernel_codification('74391d9a-125a-47f1-97f1-b79a1c5ac760', distributed).
narrative_ontology:cs_authority_grounding('74391d9a-125a-47f1-97f1-b79a1c5ac760', practice).
narrative_ontology:cs_interpretation_layer_present('74391d9a-125a-47f1-97f1-b79a1c5ac760').
narrative_ontology:cs_reading_relation('74391d9a-125a-47f1-97f1-b79a1c5ac760', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('74391d9a-125a-47f1-97f1-b79a1c5ac760', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('74391d9a-125a-47f1-97f1-b79a1c5ac760', foundational, maintenance_confers_authority).
narrative_ontology:cs_axiom_status(maintenance_confers_authority, holdable).
narrative_ontology:cs_axiom_grounding('74391d9a-125a-47f1-97f1-b79a1c5ac760', maintenance_confers_authority, instrumental).
narrative_ontology:cs_axiom('74391d9a-125a-47f1-97f1-b79a1c5ac760', foundational, every_station_bears_ma_at_duties).
narrative_ontology:cs_axiom_status(every_station_bears_ma_at_duties, holdable).
narrative_ontology:cs_axiom_grounding('74391d9a-125a-47f1-97f1-b79a1c5ac760', every_station_bears_ma_at_duties, deontological).
narrative_ontology:cs_reference_frame('74391d9a-125a-47f1-97f1-b79a1c5ac760', demonstrated_maintenance_authority).
narrative_ontology:cs_drift_state('74391d9a-125a-47f1-97f1-b79a1c5ac760', late_new_kingdom_decentralization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('74391d9a-125a-47f1-97f1-b79a1c5ac760', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, scribal_administrative_class).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, judicial_officialdom).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, corvee_laboring_peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, corvee_laboring_peasantry).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, pharaoh_bound_by_ma_at).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, judicial_officialdom).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, demonstrated_maintenance_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the order: sets the calendar of festivals and labor seasons, appoints judges and stewards, receives the accounting of land and grain. Under this arrangement his own conduct is measured by the same standard he administers — the flood's arrival, the honesty of his courts, the feeding of the dead — and the memory of the First Intermediate Period stands as proof that a failing holder of the office forfeits standing. Exit from the office does not exist; the office and the man are fused from coronation to burial.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh_bound_by_ma_at, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh_bound_by_ma_at, payer).

% Keeps the cult calendar, staffs the oracles, maintains the mortuary services, and receives the endowment lands, offerings, and grain shares that fund them. Across the generations covered here the great estates accumulate; in the later period priesthood becomes hereditary and sons succeed fathers without examination. Leaving the cult means leaving caste, kin network, and livelihood at once.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, temple_priesthood, beneficiary,
    institutional, generational, identity_locked, national).

% Runs the counting: census, tax assessment, granary ledgers, corvée rosters, correspondence. Literacy is rare and gated through years of school built on virtue maxims that rank the scribe's station above all others. A trained scribe's livelihood, marriage prospects, and standing all ride on the administrative machine continuing; stepping off the path means returning to the fields one was schooled above.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, scribal_administrative_class, beneficiary,
    organized, biographical, constrained, national).

% Viziers, nomarchs, and local court members hear disputes, fix boundaries, and punish false measure. Their tomb inscriptions compete in boasts of having fed the hungry and judged impartially, because standing in this arrangement is earned by visible fair dealing; the vizier's installation oath binds him to hear the small and the great alike. They are also reviewable — complaints travel upward, and corrupt judges lose office.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, judicial_officialdom, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, judicial_officialdom, payer).

% Farm the floodplain, deliver the assessed grain, and serve the labor seasons on canals, temples, and tombs. In return come managed floodwater, reserve granary in lean years, and a court to which grievance may be carried — the peasant speaker of the Eloquent Peasant wins his hearing by arguing in the order's own terms. Movement off the land is restricted by obligation and by the difficulty of subsisting outside the system that feeds; collective withholding happens, as when the workmen of the royal necropolis sat down at the gate over unpaid rations.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, corvee_laboring_peasantry, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, corvee_laboring_peasantry, beneficiary).

% Taken in campaigns to Nubia and the Levant and set to temple building, mining, and vineyard work inside the valleys. The circle of obligation described as 'all actors' is drawn at the border of the ordered world; these laborers fall outside it — no petition channel, no festival share, their disorder treated as natural. Escape means desert and recapture.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, foreign_war_captives, excluded,
    powerless, immediate, trapped, regional).

% Reconstruct the arrangement from stelae, papyri, tomb inscriptions, and settlement archaeology millennia later. They see the whole structure at once — including the boundary at the desert's edge and the gap between professed virtue and trial-record reality — and hold no station inside it.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, egyptological_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, temple_priesthood).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Binds an irrigation-dependent polity into reliable cooperation: assigns every station a conduct-role whose aggregate performance keeps dikes cut, granaries filled, judgments rendered, and cult served on schedule; supplies a shared vocabulary in which conduct can be praised, blamed, and adjudicated from village court to royal inscription.
% TRANSFER_FUNCTION: Moves labor and grain upward (corvée seasons, tax assessment, temple offerings) and protection, adjudication, and ritual assurance downward; distinctively in this reading, it also moves accountability upward — petitions, judicial review of officials, and the standing possibility that a failing king forfeits legitimacy.
% ABSENT_VOICES: Foreign war captives and border peoples stand outside the circle of 'all actors' — they would object that the ordered world's harmony is subsidized by their uncompensated labor, and they are not in the conversation. The rural non-literate majority speaks only through formulaic mediation; the dead are judged by the standard but shape it never.
% DISAPPEARANCE_RATIONALE: Overnight loss of the distributed-maintenance norm would leave corvée unscheduled, granaries unmanaged, disputes unadjudicated, and offices illegitimate; the flood-cycle economy stalls within a season and some replacement order — coercive or rebuilt — must be constructed from wreckage, as the First Intermediate Period previewed.
% FOUNDING_PROBLEM: After unification — and again, decisively, after the First Intermediate Period demonstrated that kings fail — how to secure cooperation across a vast irrigation polity without omnipresent coercion: generalize the maintenance burden so that order is everyone's work and no one's monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from subordinate seats: the Eloquent Peasant's petition speeches argue the justice-and-order problem from below; the Deir el-Medina strike papyri record workmen invoking owed rations as a failure of maintenance; tomb-robbery trial records show judicial process turned on the powerful. Limitation stated plainly: every surviving text passed through scribal hands, so corroboration is real but mediated — no unmediated voice from the bottom attests.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.38: the arrangement performs genuine coordination — a flood-cycle agrarian economy needs scheduled labor, granary redistribution, and trusted adjudication — while carrying real but bounded extraction: corvée and grain assessment weigh hardest at the bottom, and station-language quietly naturalizes the ladder it presumes. Suppression 0.46 is authored as a RAW STRUCTURAL property (only extractiveness is scaled by directionality and scope downstream): courts, corvée officers, oath machinery, plus an eschatological tribunal extending enforcement past death, offset by live petition channels and the fact that little conceptual room for dissent meant little machinery spent suppressing it. Theater 0.28: tomb-autobiography virtue formulas and royal Ma'at rhetoric are performative, but judgment, granary accounting, and festival logistics were load-bearing. Accessibility_collapse 0.60: within the Egyptian lifeworld, living outside Ma'at was barely thinkable — yet the framework itself admits rival interpretations (this very kernel is contested across readings) and its protections stopped at the border. Resistance 0.34: strikes (the necropolis workmen's sit-down), petitions, corruption, tomb robbery, and periodic regional defection — nearly always argued in Ma'at's own vocabulary rather than against it. Temporal grid: one shared grid, all three tracked metrics authored at every point {0,20,40,60,80,100}, indexing century-scale phases from early Middle Kingdom (0) to the threshold of the Third Intermediate Period (100). The mid-interval dip marks the Second Intermediate Period, when central enforcement capacity collapsed faster than extraction did; the later rise tracks imperial wealth and deepening stratification; the terminal easing reflects fragmentation of state extraction capacity alongside growing temple capture.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently, and the royal seat computes as neither. From the corvee_laboring_peasantry seat the same norm that promises order delivers a fixed station and the heaviest dues; petition channels are real but discretionary, and collective leverage (the strike) is the one lever that moved the center. From the temple_priesthood and scribal_administrative_class seats the arrangement is livelihood and standing — its maintenance IS their income — and the later hereditary closure reads, from inside, as piety rather than capture. The pharaoh_bound_by_ma_at seat is uniquely dual: it administers the standard and is measured by it, so from the throne the arrangement is the visible machinery of order while from below it is the reason the dues arrive. Identity-lock is load-bearing here: priestly and royal selves are fused with office (exit unthinkable), and the Amarna rupture shows what happens when the apex attempts to redefine the order itself — the framework absorbed the break and restored itself. The engine computes per-seat classifications from these structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (temple_priesthood, scribal_administrative_class, judicial_officialdom) derive low directionality — subsidized seats collecting standing and material flow. Declared victim corvee_laboring_peasantry derives high directionality, amplified by trapped exit. The royal seat needs no override: its dual declaration (agenda_setter + payer) already encodes the near-symmetric ambivalence a single-role derivation would miss, which is why no directionality_overrides entries are authored. foreign_war_captives deliberately appear in NO beneficiary/victim declaration — their exclusion from the circle of obligation is carried by the ordered_world_boundary omega rather than by a directionality value, which is exactly why that omega matters: the arrangement's low ε is computed over the population it counted.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Calling the arrangement a snare would erase the real coordination: without scheduled corvée, granary redistribution, and adjudicated dispute resolution, the flood-cycle economy fails and the world rearranges (Q5 verdict). Calling it a rope would erase the asymmetric weight of station-dues and the accumulating concentration of material flow in temple estates — the receipt surface names a capturing seat, and fixing is prohibitive for the agenda-setter relative to its benefit. Mandatrophy is NOT resolved: the founding problem (binding an irrigation polity into cooperation without omnipresent coercion) stayed live across the entire interval; nothing here is vestige or performance propping up a dead function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the maat_order_principle kernel — the distributed_maintenance_reading. How would the beneficiary/victim structure and epsilon change under the sibling readings?',
    'Comparative coding of the textual corpora each reading privileges: royal annals and coronation inscriptions for divine_mandate_reading; reciprocal-obligation and famine-relief texts for reciprocity_reading; wisdom literature and judicial records for this reading — tracking which actors are held accountable in each corpus.',
    'Under divine_mandate_reading the apex becomes unaccountable by definition and epsilon rises sharply with extraction concentrated on subjects; under reciprocity_reading the apex becomes a bound counterparty but remains the sole hub, giving intermediate epsilon. This file''s epsilon (0.38) is valid only for the distributed reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: sibling readings redistribute accountability and shift epsilon.').

omega_variable(
    station_system_naturalness,
    'Does ''proper conduct in your station'' attach duties to pre-given stations, or does the station-list itself perform the hierarchical work — does the norm legitimate the inequality it presupposes?',
    'Track station mobility in the record: promotion inscriptions and self-made careers alongside hereditary closure patterns, asking whether Ma''at discourse tracks mobility or resists it.',
    'If stations are load-bearing fictions, part of the measured extraction is hierarchy-naturalization beyond conduct-demand and epsilon trends upward; if stations are contingent descriptions the arrangement approaches pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(station_system_naturalness, conceptual, 'Whether station-language naturalizes hierarchy or merely describes roles.').

omega_variable(
    upward_accountability_efficacy,
    'Did the distributed accountability channels — petition, judicial review, oracle inquiry — actually constrain official conduct, or operate as pressure valves that preserved the arrangement while deflecting redress?',
    'Case-track petition and strike outcomes across the interval: redress granted versus deferred versus punished (the Eloquent Peasant outcome, necropolis strike settlements, oracle-decreed land judgments).',
    'If valves, effective extraction on the laboring population exceeds the authored 0.38 and the arrangement drifts toward heavier extraction; if genuine constraint, the low family-relative epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(upward_accountability_efficacy, empirical, 'Efficacy of the accountability mechanisms this reading rests on.').

omega_variable(
    ordered_world_boundary,
    'Does ''all actors from Pharaoh to commoner'' include the foreign populations whose labor the ordered world used, or does distributed responsibility stop at the border while material extraction crosses it?',
    'Analyze whether Ma''at-category obligations (maintenance, protection, judgment) are ever extended to captive and foreign labor in texts and practice, versus categorical outsider-status (pejorative border epithets, branded captives, exclusion from festival shares).',
    'If outsiders are excluded, this reading''s low epsilon is partly subsidized by extraction the arrangement does not count; at imperial scope the effective extraction of the whole system is materially higher than any single-reading measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordered_world_boundary, conceptual, 'Boundary problem: the universality claim stops at the ordered world''s edge.').

omega_variable(
    eschatological_enforcement_reach,
    'How much of the arrangement''s hold is structural (courts, corvée officers, overseers) versus internalized-eschatological (the heart weighed against the forty-two declarations after death, making evasion impossible even in death)?',
    'Compare conduct where structural enforcement was absent — tomb-robbing waves despite curses, late-period local defiance — against professed conformity in unobserved contexts; treat the widening availability of funerary assurance scrolls as purchased internalization.',
    'If the eschatological tribunal carries substantial load, suppression is understated by structural measures alone and exit is trapped even eschatologically — raising effective suppression without any added enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eschatological_enforcement_reach, conceptual, 'Structural versus internalized-eschatological suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(maat_tr_t0, observed).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(maat_tr_t20, observed).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__distributed_maintenance_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(maat_tr_t40, observed).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__distributed_maintenance_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(maat_tr_t60, observed).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__distributed_maintenance_reading, theater_ratio, 80, 0.31).
narrative_ontology:measurement_basis(maat_tr_t80, observed).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__distributed_maintenance_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(maat_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(maat_be_t0, observed).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement_basis(maat_be_t20, observed).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement_basis(maat_be_t40, observed).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement_basis(maat_be_t60, observed).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 80, 0.43).
narrative_ontology:measurement_basis(maat_be_t80, observed).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(maat_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(maat_su_t0, observed).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(maat_su_t20, observed).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement_basis(maat_su_t40, observed).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(maat_su_t60, observed).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement_basis(maat_su_t80, observed).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 100, 0.46).
narrative_ontology:measurement_basis(maat_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Ma'at' conflates at least three structurally distinct arrangements of one kernel. This file holds the distributed_maintenance_reading with epsilon 0.38 authored for the distributed arrangement alone; maat_order_principle__divine_mandate_reading holds the inherent-apex arrangement (highest epsilon; elite accountability dissolved by definitional infallibility); maat_order_principle__reciprocity_reading holds the contractual-mutuality arrangement (intermediate epsilon; the apex bound but as sole counterparty). The readings differ in WHERE accountability attaches, not in the topic; each is separately ε-invariant and linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
