% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause, Narrow Originalist Reading: Federal Power Limited to Trade Crossing State Lines
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the narrow_originalist reading of the contested
 *   commerce_clause_scope kernel: the rule that 'commerce among the states'
 *   means trade crossing state lines, that 'regulate' means make regular
 *   rather than restrict, and that federal power therefore extends only to
 *   removing state-imposed barriers to interstate trade and keeping
 *   commercial rules uniform. The rule binds Congress by design, is
 *   administered by the Supreme Court, reserves intrastate regulatory
 *   jurisdiction to the states, and — as its enforcement matures — strips
 *   federal protection from populations in recalcitrant states. The epsilon
 *   referent is this reading's own arrangement (the narrow rule as operative
 *   constraint), not the broad regime it contests; values are
 *   reading-indexed. The reading's own lights would score the rule near zero
 *   extraction — it withholds power rather than taking value — while the
 *   descriptive metric registers the real delta the same structure imposes on
 *   unprotected populations; claim and metrics are authored independently,
 *   and that divergence is signal, not error. The constraint is claimed as a
 *   hybrid coordination/extraction structure: the barrier-removal
 *   coordination function is genuine and historically primary, and the same
 *   scope line that delivers it strips protection from identifiable payers.
 *
 * KEY AGENTS:
 *   - supreme_court: Agenda setter (institutional/constrained) — administers the reading, strikes statutes that reach past trade crossing state lines
 *   - congress: Primary target of the binding (institutional/constrained) — the jurisdiction the reading removes is Congress's
 *   - state_governments: Primary beneficiary (institutional/constrained) — collects the transferred regulatory jurisdiction
 *   - local_businesses: Beneficiary (moderate/mobile) — exempt from federal regulation of intrastate activity
 *   - interstate_merchants_and_carriers: Beneficiary (organized/mobile) — the Clause's protected class; barrier removal and uniform rules, with a mixed interest in national harmonization
 *   - originalist_legal_movement: Beneficiary (organized/mobile) — collects interpretive authority, appointments, and doctrinal authorship
 *   - civil_rights_claimants_in_recalcitrant_states: Payer (powerless/trapped) — federal protection stripped, state protection absent
 *   - workers_in_recalcitrant_states: Payer (powerless/constrained) — lose federal wage/hour/safety floors
 *   - communities_bearing_unregulated_local_pollution: Payer (powerless/trapped) — local polluters beyond federal reach
 *   - federal_regulatory_agencies: Excluded (institutional/trapped) — jurisdiction removed, methodologically outside the interpretive conversation
 *   - national_civil_rights_organizations: Excluded (organized/trapped) — locked out of the originalist method conversation
 *   - analytical observer: the classification engine, seeing the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.45).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.5).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.45).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause, Narrow Originalist Reading: Federal Power Limited to Trade Crossing State Lines").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, 'da5b2c45-d422-4c67-98a6-a82ec9283439').
narrative_ontology:cs_kernel_codification('da5b2c45-d422-4c67-98a6-a82ec9283439', fixed_text).
narrative_ontology:cs_authority_grounding('da5b2c45-d422-4c67-98a6-a82ec9283439', lineage).
narrative_ontology:cs_interpretation_layer_present('da5b2c45-d422-4c67-98a6-a82ec9283439').
narrative_ontology:cs_reading_relation('da5b2c45-d422-4c67-98a6-a82ec9283439', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_reading_relation('da5b2c45-d422-4c67-98a6-a82ec9283439', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('da5b2c45-d422-4c67-98a6-a82ec9283439', foundational, commerce_means_trade_crossing_state_lines).
narrative_ontology:cs_axiom_status(commerce_means_trade_crossing_state_lines, holdable).
narrative_ontology:cs_axiom_grounding('da5b2c45-d422-4c67-98a6-a82ec9283439', commerce_means_trade_crossing_state_lines, empirically_contingent).
narrative_ontology:cs_axiom('da5b2c45-d422-4c67-98a6-a82ec9283439', foundational, regulate_means_make_regular_not_restrict).
narrative_ontology:cs_axiom_status(regulate_means_make_regular_not_restrict, holdable).
narrative_ontology:cs_axiom_grounding('da5b2c45-d422-4c67-98a6-a82ec9283439', regulate_means_make_regular_not_restrict, empirically_contingent).
narrative_ontology:cs_axiom('da5b2c45-d422-4c67-98a6-a82ec9283439', secondary, state_police_power_is_default_authority).
narrative_ontology:cs_axiom_status(state_police_power_is_default_authority, holdable).
narrative_ontology:cs_axiom_grounding('da5b2c45-d422-4c67-98a6-a82ec9283439', state_police_power_is_default_authority, conventional).
narrative_ontology:cs_reference_frame('da5b2c45-d422-4c67-98a6-a82ec9283439', ratification_era_trade_facilitation_power).
narrative_ontology:cs_drift_state('da5b2c45-d422-4c67-98a6-a82ec9283439', contemporary_originalist_majority_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('da5b2c45-d422-4c67-98a6-a82ec9283439', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, interstate_merchants_and_carriers).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, originalist_legal_movement).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, congress).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, workers_in_recalcitrant_states).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, communities_bearing_unregulated_local_pollution).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, dual_federalism).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, state_police_power_primacy).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, original_meaning_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides what the Commerce Clause permits, case by case, and under this reading strikes down federal statutes that reach activity outside trade crossing state lines. Its interpretive majority determines how much force the reading has; justices serve for decades and the institution cannot decline the boundary-drawing role. It is bound by its own precedents in both directions — the broad regime it inherited and the narrow line it is reviving — so its exit from the contest is constrained rather than open.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Enacts nationwide rules on labor, commerce, environment, and civil rights. Under this reading its authority stops at trade crossing state lines: statutes reaching purely local activity fall, and regulatory jurisdiction over economic and social life inside the states reverts to the states. It retains the interstate-trade, taxing, and spending powers, but its way out of the limitation — constitutional amendment — requires supermajorities never assembled for this purpose. The limitation binds it by design; removing federal jurisdiction is what the reading does.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, congress, payer,
    institutional, biographical, constrained, national).

% Retain default regulatory authority over economic and social life within their borders: licensing, labor conditions, land use, professional regulation, the police powers generally. The reading reserves to them the jurisdiction the federal government would otherwise exercise, with the licensing revenue, political credit, and policy experimentation that come with it. They remain bound by the federal framework and by the duty not to burden interstate trade, and they actively litigate and appoint to hold the boundary where it is.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Operate wholly inside one state and, under this reading, answer to their home state's rules alone — federal labor, environmental, and civil rights statutes do not reach purely intrastate activity. If a neighboring state's regime is cheaper they can relocate, which disciplines their home state's regulatory ambitions. Businesses trading across state lines do not share the exemption.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    moderate, biographical, mobile, local).

% Move goods, services, and people across state lines. They are the class the Clause was written to protect: the federal government strips state-imposed barriers from their trade and keeps commercial rules uniform so a transaction valid in one state holds across the network. Their interest is split — barrier removal pulls them toward this reading, but the prospect of one national regulatory regime in place of fifty pulls them toward the broad reading.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, interstate_merchants_and_carriers, beneficiary,
    organized, biographical, mobile, continental).

% Face exclusion from public accommodations, employment, and housing by businesses operating wholly within their state. Under this reading federal civil rights statutes cannot reach those businesses, and protection depends entirely on whether their own state will act — in states whose politics produced the exclusion, it will not. Leaving the state is costly and does not reliably escape the treatment.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states, payer,
    powerless, biographical, trapped, regional).

% Work for employers beyond the reach of federal wage, hour, and safety statutes under this reading. Their floor is whatever their state enacts, and states competing to keep employers have reason to keep that floor low. Relocation to a stronger-protection state is possible but expensive, and the low-wage jobs they hold are the least compatible with moving.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, workers_in_recalcitrant_states, payer,
    powerless, biographical, constrained, regional).

% Live near pollution sources whose originating activity is local and not part of trade crossing state lines, so federal environmental statutes cannot reach them under this reading. Their recourse is their own state's law or nuisance actions against sources across a state line — slower and weaker than federal enforcement. They cannot relocate away from the air they breathe or the water they draw.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, communities_bearing_unregulated_local_pollution, payer,
    powerless, generational, trapped, regional).

% Administer nationwide environmental, labor, and health programs whose jurisdiction over purely intrastate activity this reading removes. Their expertise and their defense of comprehensive regulation carry little weight in the interpretive conversation that fixes the Clause's meaning, which is conducted in ratification-era semantic terms; they meet the reading in litigation defending their statutes rather than as participants in the method that decides them. They cannot abandon their statutory mandates.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_regulatory_agencies, excluded,
    institutional, biographical, trapped, national).

% Litigate and organize for federal protection against discrimination. The reading's adherents treat their constitutional arguments as results-driven rather than principled, and the constituencies they represent hold no seat in the originalist method conversation; they encounter the reading as an obstacle in court rather than as a position contestable on its own methodological terms. Their mandate binds them to the populations whose protection is at stake.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_civil_rights_organizations, excluded,
    organized, generational, trapped, national).

% A network of scholars, lawyers, and judges supplying the reading's interpretive program: founding-era semantics, ratification debates, corpus linguistics. As the reading gains force they collect appointments, clerkship pipelines, journal influence, and doctrinal authorship. Their position is portable — the same methodological capital transfers to other clauses and other contests if this one closes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, originalist_legal_movement, beneficiary,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes state-imposed barriers to trade crossing state lines and maintains uniform commercial rules for that trade. The underlying collective action problem is real: each state gains from favoring in-state merchants if others' barriers stand, no merchant can contract around fifty regulatory regimes, and only a federal power can strip the barriers and keep the rules uniform. Under this reading the federal government performs exactly that function and nothing more; everything inside state lines stays with the states.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction over intrastate economic and social life from the federal government to state governments, moves exemption from federal regulation to businesses operating within a single state, and — as the reading's enforcement strikes federal statutes — moves the costs of unregulated activity onto the populations federal protection would otherwise have covered: civil rights claimants, low-wage workers, and pollution-exposed communities.
% ABSENT_VOICES: The populations whose federal protection the reading eliminates — civil rights claimants in recalcitrant states, low-wage workers, pollution-exposed communities — are largely absent from the interpretive conversation that sets the Clause's meaning, which is conducted among judges, scholars, and advocates in ratification-era semantic terms. Federal regulatory agencies appear only as litigants defending statutes, and their expertise-based arguments are methodologically discounted. Those voices would object that the reading converts their protection into a states' policy experiment they did not consent to; their absence from the method conversation is structural, not incidental.
% DISAPPEARANCE_RATIONALE: If the narrow reading stopped binding overnight, federal statutes would re-expand over intrastate economic and social life: national labor, environmental, and civil rights standards would reach local businesses, state governments would lose the jurisdiction and licensing authority the reading reserves to them, and the fifty-regime landscape would consolidate toward federal uniformity. The arrangement's beneficiaries hold their gains only while the reading holds, so its disappearance rearranges the federal system rather than leaving it as-is.
% FOUNDING_PROBLEM: Two-layered. The Clause's founding problem was state-imposed barriers fragmenting interstate trade — each state taxing, tariffing, and favoring in-state merchants, with no uniform commercial rules and no neutral arbiter. The modern reading's founding problem is federal regulatory power grown past its enumeration, pressing on state sovereignty and individual liberty. The narrow reading claims to solve both at once: federal power sufficient to strip state barriers from interstate trade, and no more than that.
% FOUNDING_PROBLEM_CORROBORATION: The founding-era barrier problem is corroborated from outside any modern benefiting party by the ratification records and the early trade-barrier litigation (the Gibbons-era steamboat and navigation conflicts). The modern overreach problem splits on coalition lines: state attorneys general and the originalist academy attest it is live; living-constitutionalist scholars, the civil rights bar, and the federal regulatory agencies attest it is constructed, or that its solution costs more than the condition. No source outside the beneficiary set attests both layers, and that split is itself the signal.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45. The rule's coordination core — stripping state barriers from interstate trade and keeping commercial rules uniform — is genuine, but the same scope line that delivers it removes federal protection from identifiable populations and transfers their regulatory jurisdiction to the states; as enforcement matures, that delta materializes. Suppression is authored at 0.50 as a raw structural property, unscaled: the rule's active force is judicial (statutes struck, jurisdiction returned), and it suppresses directionally — closing the federal-regulatory path while leaving state-regulatory paths fully open. Theater is 0.20: the reading's activity has shifted over the interval from scholarly performance (originalism as academic identity through the 1980s) toward operative doctrine, so the performative share falls while realized extraction and enforcement rise. Accessibility collapse is 0.30: alternatives remain open — state regulation, the taxing and spending powers, interstate compacts, the common law; the rule closes one path, not all of them. Resistance is 0.75: Congress, the federal agencies, the civil rights and labor bar, and roughly half the academy actively contest the reading, and every constituency of the modern regulatory state has a stake against it. The three measurement series share one grid of six points (t=0 maps to about 1975, t=50 to 2025): extraction and enforcement rise monotonically with the reading's institutional purchase (the Lopez-era revival through the current originalist majority), and theater falls as scholarship becomes doctrine. The victim seats are powerless with state-bounded harms, so coalition potential is weak — their dockets diverge, and the one historic coalition of these constituencies (the New Deal coalition) was answered by the broad reading this one contests.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same doctrine. From the state government seat the rule is a sovereignty shield: jurisdiction retained, experimentation protected, federal power held at the trade line — an arrangement the states defend and appoint for. From the civil rights claimant seat the same line is the removal of the only enforcement that ever reached recalcitrant states — pure loss, no exit, no compensating benefit. From the interstate merchant seat the rule delivers the Clause's original benefit while threatening its modern one: barrier removal yes, fifty regimes where one would do no. From the Congress seat it is a jurisdictional amputation by design. Inter-institutionally, the Court administers a boundary it cannot decline to draw; Congress bears a limitation it can escape only by amendment; the agencies lose jurisdiction they cannot abandon; the states gain jurisdiction they actively litigate to keep. Same nominal institutional tier, four different constraints — the engine computes the divergence from role, power, and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: state governments collect the transferred jurisdiction (low d, and they act to keep it); local businesses collect the intrastate exemption and hold mobile exit, which pushes them toward the beneficiary end; the originalist movement collects interpretive authority and appointments. Victim declarations: civil rights claimants and pollution-exposed communities hold trapped exits and sit near the full-target end; recalcitrant-state workers are constrained and sit high; Congress sits near full-target despite institutional power because the binding is the rule's designed function and its exit — amendment — is prohibitive. Interstate merchants are the one seat the derivation over-simplifies: declared beneficiary with mobile exit, they also lose the prospect of federal harmonization under this reading, so their true position sits nearer the middle than the derivation alone would place it; the mixed interest is recorded here rather than as an override, since the override surface is keyed by power atom and would misstate the other organized seats. Scope note: the rule operates at national scope while the harms are state-bounded, and the genuinely hard verification margin — whether local activity affects interstate trade — is exactly the margin where the three readings diverge. Receipt: the rule's gains — transferred jurisdiction and the political credit attached to it — land demonstrably on state_governments; local businesses benefit but diffusely, and the movement's gains are status rather than jurisdiction. Fixing is cheap: reversal is a five-vote doctrinal matter, as 1937 demonstrated; the arrangement persists because its beneficiaries hold the appointing pipeline and the agenda, not because removal is expensive.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabels. Calling the rule pure coordination would erase the delta: the scope line that frees local businesses is the same line that strips federal protection from recalcitrant-state populations, and the victim declarations block that erasure. Calling it pure extraction would erase the founding function: barrier removal was the Clause's original purpose, its application to modern state barriers (occupational licensing, certificate-of-need laws) is live, and the coordination story is not cover. The hybrid claim holds both. Mandatrophy: the founding problem has two layers — the trade-barrier layer is demonstrably live, so no mandatrophy resolution is declared; the federal-overreach layer is contested, and a wholesale replacement of the broad regime by this reading would force re-examination of whether the checking function still tracks a live problem. Identity-lock note: part of the reading's enforcement coalition is held by professional and ideological identity fusion — judges and scholars whose self-concept is constituted by originalist fidelity — so if the semantic premises (the two foundational axioms) were refuted by corpus-linguistic evidence, the coalition could fracture faster than doctrinal inertia alone predicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the narrow_originalist reading of the commerce_clause_scope kernel; how much of this story''s structure — victim set, beneficiary set, extractiveness, classification — is reading-specific rather than kernel-level?',
    'Generate the sibling stories (broad_effects_test, intermediate_channels) and diff the structural data: the broad reading moves the regulated set from interstate traders to the whole national economy and raises federal extractiveness; the intermediate reading splits the difference with limiting principles. The points where the three stories'' beneficiary and victim declarations diverge are exactly the reading-specific structure.',
    'Classification and seat structure computed here hold only for this reading; pooling the three readings'' data would violate epsilon-invariance and misstate every seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    facilitation_semantics_contest,
    'Is the reading''s most load-bearing semantic premise accurate — that in founding-era usage ''regulate commerce'' meant make regular (facilitate) and did not include restriction or prohibition?',
    'Corpus linguistics and founding-era usage studies; the premise is falsifiable by evidence that ''regulate'' was standardly used to cover prohibitory measures (embargoes, navigation acts) described as regulations of commerce.',
    'If ''regulate'' included restriction, the reading''s limiting principle collapses toward the intermediate or broad readings, and the narrow victim set and low federal footprint computed here go with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(facilitation_semantics_contest, empirical, 'Whether ''regulate equals make regular'' is a historically accurate semantic claim.').

omega_variable(
    commerce_scope_semantics_contest,
    'Does ''commerce among the states'' at ratification mean only trade crossing state lines, or did it extend to the broader economic activity that produces trade — navigation, production, insurance?',
    'Ratification-era dictionaries, Federalist usage, founding-era regulation of navigation and production, and early Court practice (Gibbons).',
    'If commerce reached production and navigation broadly, the trade-crossing-lines boundary is drawn too tight and the victim set — who loses federal protection — is mis-specified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commerce_scope_semantics_contest, empirical, 'Semantic scope of ''commerce'' at ratification.').

omega_variable(
    state_benefit_heterogeneity,
    'Do state governments uniformly benefit from the rule, or does a substantial subset prefer the federal regulatory floors the rule removes?',
    'State legislative and attorney-general behavior: states that adopt above-federal standards, or litigate in defense of federal authority, reveal preference heterogeneity inside the beneficiary class.',
    'If a large subset of states prefers federal floors, the beneficiary declaration splits — those states sit nearer the target end (they lose a backstop they relied on) and the state seat''s directionality becomes heterogeneous rather than uniformly low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_benefit_heterogeneity, empirical, 'Whether the state-government beneficiary class is uniform.').

omega_variable(
    protection_loss_registration,
    'Can the reading''s own normative framework register the loss of federal civil rights, labor, and environmental protection as a cost at all, or does that loss fall outside what its lights can see — the reading scores the arrangement near zero extraction because it withholds power rather than taking value?',
    'Comparative institutional analysis: outcomes for the affected populations under state-only enforcement versus federal enforcement; persistent divergence shows the loss is a real cost the reading''s self-assessment cannot register.',
    'Determines whether the arrangement classifies as coordination carrying an asymmetric delta through one structure, or as pure coordination whose payers sit outside its frame; the divergence between the reading''s self-scored extraction and the descriptive metric is itself the measurement the corpus exists to take.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_loss_registration, conceptual, 'Whether the reading''s framework can register the protection loss its operation imposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_scope__narrow_originalist, theater_ratio, 10, 0.28).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__narrow_originalist, theater_ratio, 20, 0.25).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_scope__narrow_originalist, theater_ratio, 30, 0.24).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_scope__narrow_originalist, theater_ratio, 40, 0.22).
narrative_ontology:measurement(comm_tr_t50, commerce_clause_scope__narrow_originalist, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(comm_be_t10, commerce_clause_scope__narrow_originalist, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__narrow_originalist, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(comm_be_t30, commerce_clause_scope__narrow_originalist, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(comm_be_t40, commerce_clause_scope__narrow_originalist, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(comm_be_t50, commerce_clause_scope__narrow_originalist, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__narrow_originalist, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(comm_su_t10, commerce_clause_scope__narrow_originalist, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__narrow_originalist, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(comm_su_t30, commerce_clause_scope__narrow_originalist, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(comm_su_t40, commerce_clause_scope__narrow_originalist, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(comm_su_t50, commerce_clause_scope__narrow_originalist, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, resource_allocation).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Commerce Clause' covers three structurally distinct constraints — one per reading of the commerce_clause_scope kernel. This file instantiates the narrow reading only: its epsilon is reading-indexed over the narrow rule's own operation, and its victim set (recalcitrant-state populations, plus Congress by design) is specific to this reading. broad_effects_test instantiates a different constraint — the whole national economy as the regulated set, higher federal extractiveness — and intermediate_channels sits between with limiting principles. The operative upstream reading since 1937 is broad_effects_test; this reading pressures it (Lopez, Morrison, the originalist majority) without displacing it. Each sibling file links the others through affects_constraints; pooling their data would violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
