% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Legal Partition and State Recognition (Partition Reading)
 *   domain: political_theory/international_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   territorial_legitimacy: the partition reading, under which territorial
 *   title flows from international legal acts and collective state
 *   recognition — Resolution 181 as dispositive precedent, the
 *   armistice/Green Line as the binding frame, and both Israeli and
 *   Palestinian statehood legitimate within recognized borders while
 *   construction beyond the 1967 lines is unlawful. The ε referent is fixed:
 *   the standing arrangement under contest is the partition-legitimacy regime
 *   itself, assessed by this reading's own lights — the reading endorses the
 *   frame, and that endorsement does not zero ε, because the frame
 *   demonstrably imposes costs on identifiable classes (claimants beyond the
 *   line, refugees whose return claims are subordinated to finality,
 *   communities divided by the line). Constraint-family decomposition per the
 *   ε-invariance principle: 'territorial legitimacy' is one colloquial label
 *   covering three structurally distinct claims with different title-sources
 *   and therefore different victim/beneficiary sets and different ε. This
 *   file carries the partition reading; the security-necessity reading (which
 *   conditionally legitimizes holdings beyond the line) and the
 *   indigenous-continuity reading (under which the 1947-49 transfer is the
 *   central injury and both states' title is contestable, driving ε sharply
 *   upward over the same referent) are separate files linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship):
 *   green_line_israel — dual-positioned principal (powerful/constrained):
 *   collects recognition rents within the lines, bears restraint costs beyond
 *   them; palestine_as_recognized_state — burdened beneficiary
 *   (moderate/trapped): recognition without enforcement;
 *   west_bank_settlement_movement — primary target
 *   (organized/identity_locked); palestinian_refugees_descendants —
 *   structural loser of the founding act (powerless/trapped);
 *   great_power_patrons — administrator-beneficiaries (powerful/arbitrage);
 *   un_security_council and un_general_assembly — administering organs
 *   (institutional/identity_locked); icj_and_international_courts —
 *   analytical observer; binational_confederal_advocates — excluded voice;
 *   green_line_divided_communities and jordan_lebanon_host_states — residual
 *   cost-bearers.
 *
 * KEY AGENTS:
 *   - west_bank_settlement_movement: primary target (organized/identity_locked) — bears the frame's delegitimation, demolition exposure, and evacuation scenarios beyond the line
 *   - palestinian_refugees_descendants: structural loser of the founding act (powerless/trapped) — return claims subordinated to border finality across a diaspora
 *   - green_line_israel: dual-positioned principal (powerful/constrained) — collects recognition within recognized lines, pays restraint and litigation costs beyond them
 *   - palestine_as_recognized_state: burdened beneficiary (moderate/trapped) — gains legal personality while actual control falls far short of recognized borders
 *   - great_power_patrons: administrator-beneficiaries (powerful/arbitrage) — convert enforcement discretion into leverage over both parties
 *   - un_security_council: enforcing administrator (institutional/identity_locked) — holds coercive instruments, rations them by veto
 *   - un_general_assembly: founding and ceremonial administrator (institutional/identity_locked) — adopted 181, maintains the frame by recurring symbolic majorities
 *   - icj_and_international_courts: analytical observer (institutional/analytical) — translates the frame into judicial findings
 *   - binational_confederal_advocates: excluded voice (organized/constrained) — contends with the frame's dominance from outside the table
 *   - green_line_divided_communities: line-cost bearer (powerless/trapped) — lives astride the 1949 armistice line without consultation
 *   - jordan_lebanon_host_states: residual cost-bearer (organized/constrained) — hosts deferred populations while collecting treaty and patronage returns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.45).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.6).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Legal Partition and State Recognition (Partition Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '5a846383-c8d9-4b2d-8761-08cfe5a28b1e').
narrative_ontology:cs_kernel_codification('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', formalized).
narrative_ontology:cs_authority_grounding('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', lineage).
narrative_ontology:cs_interpretation_layer_present('5a846383-c8d9-4b2d-8761-08cfe5a28b1e').
narrative_ontology:cs_reading_relation('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_reading_relation('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', foundational, recognition_constitutes_sovereign_title).
narrative_ontology:cs_axiom_status(recognition_constitutes_sovereign_title, holdable).
narrative_ontology:cs_axiom_grounding('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', recognition_constitutes_sovereign_title, conventional).
narrative_ontology:cs_axiom('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', secondary, green_line_finality_binding).
narrative_ontology:cs_axiom_status(green_line_finality_binding, holdable).
narrative_ontology:cs_axiom_grounding('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', green_line_finality_binding, conventional).
narrative_ontology:cs_reference_frame('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', resolution_181_recognition_finality).
narrative_ontology:cs_drift_state('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5a846383-c8d9-4b2d-8761-08cfe5a28b1e', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, green_line_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestine_as_recognized_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, great_power_patrons).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, west_bank_settlement_movement).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees_descendants).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, jordan_lebanon_host_states).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, green_line_divided_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, un_general_assembly).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, jordan_lebanon_host_states).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, green_line_israel).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestine_as_recognized_state).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, un_resolution_181_dispositive_authority).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, uti_possidetis_border_finality).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, charter_system_collective_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts resolutions fixing and defending the recognized-border frame and can authorize enforcement, but permanent-member vetoes decide which violations meet consequences. It holds the regime's coercive instruments — sanctions authorization, peacekeeping mandates — and rations them by consensus. Its own standing is bound up with the charter system it administers; operating outside that frame would dissolve the basis of its authority.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, un_security_council, agenda_setter,
    institutional, generational, identity_locked, global).

% Adopted the 1947 partition recommendation and annually reaffirms the recognized-line framework by overwhelming majorities. Its resolutions carry moral and precedential weight without enforcement power; the recurring symbolic vote calendar keeps the frame publicly alive. Its institutional standing grows with the authority of the frame it reaffirms.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, un_general_assembly, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, un_general_assembly, beneficiary).

% Holds uncontested recognized statehood within the armistice lines — membership, treaties, and legal personality all flow from the recognition frame. Beyond the lines it builds and administers communities that the same frame classifies as unlawful, exposing it to court jurisdiction, delegitimation campaigns, and withdrawal demands it resists. Leaving the frame would cost it the recognition that constitutes its legal existence.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, green_line_israel, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, green_line_israel, payer).

% Gains legal personality, organizational membership, and treaty capacity through the recognition track while its actual control falls far short of its recognized borders under prolonged military occupation. It pursues statehood through the very machinery whose enforcement never reaches it; abandoning the track would forfeits its principal claim to exist as a state.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestine_as_recognized_state, beneficiary,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestine_as_recognized_state, payer).

% Extend or withhold recognition, veto or permit enforcement, and broker the negotiating process, converting that discretionary position into leverage over both parties and standing with clients. Shielding aligned violators and pressuring misaligned ones is the regime's de facto enforcement policy; they pay credibility costs when the gap between the stated rule and the administered outcome widens.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, great_power_patrons, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, great_power_patrons, agenda_setter).

% Builds and inhabits communities beyond the 1967 line that the recognition frame classifies as unlawful, facing demolition exposure, labeling and boycott campaigns, and eventual evacuation scenarios. Members' attachment to the specific hilltops is ideological and religious; relocation registers as betrayal of a sacralized project rather than a negotiable cost.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, west_bank_settlement_movement, payer,
    organized, generational, identity_locked, regional).

% Descendants of those displaced in the 1947-49 war hold return claims that the line-finality frame subordinates to a negotiated package most will never access; citizenship limbo persists across host countries. The frame prices their loss in compensation terms while closing the practical door their claim names.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees_descendants, payer,
    powerless, generational, trapped, global).

% Carry multi-generational refugee populations whose integration or return the frozen frame defers indefinitely, straining economies and domestic politics. In exchange, the frame's land-for-peace track delivered recognized treaties and normalized borders to Jordan and sustained international patronage for Lebanon.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, jordan_lebanon_host_states, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, jordan_lebanon_host_states, beneficiary).

% Villages and pastoral communities split or stranded by the armistice line — families cut across it, grazing routes severed, municipal identity assigned by a line drawn over their heads in 1949. They were never consulted then and sit outside every negotiating frame now.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, green_line_divided_communities, payer,
    powerless, generational, trapped, local).

% Scholars and movements proposing one shared state or confederal structures argue that freezing the two-state frame perpetuates the underlying injury, but they are marginal to official diplomacy, thinly funded, and seated at no negotiating table. The frame's dominance is precisely what their proposals contend against.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, binational_confederal_advocates, excluded,
    organized, generational, constrained, regional).

% Issues advisory opinions and rulings on the separation barrier, settlement legality, and occupation law, translating the recognized-line frame into judicial findings. Courts decide nothing territorially themselves but supply the legal record every other seat litigates with.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, icj_and_international_courts, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, great_power_patrons).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a bilateral title war between two national movements into an adjudicable legal question by fixing mutually recognizable boundaries: once lines are internationally fixed, each movement can hold statehood, sign treaties, join organizations, and plan infrastructure without relitigating first possession. Third-party fixation substitutes for an impossible bilateral agreement.
% TRANSFER_FUNCTION: Moves recognition — and the material goods recognition unlocks (treaty access, organizational membership, aid, trade preferences, legal standing before courts) — to actors whose holdings conform to recognized lines, and moves illegitimation costs onto actors holding or building beyond them. At the founding it also allocated territorial title according to the 1947 map without the consent of roughly half the resident population.
% ABSENT_VOICES: The resident population of the Mandate was never consented in 1947 — the plan passed over the objection of the Arab Higher Committee and neighboring states, with no plebiscite. Currently excluded: binational and confederal advocates, foreclosed from the two-state conversation; refugee communities, represented only indirectly and absent from final-status bargaining; and communities physically divided by the armistice line whose daily reality the legal line ignores.
% DISAPPEARANCE_RATIONALE: The recognition architecture collapses overnight: both states' legal foundations revert to raw possession contests; every treaty, aid agreement, and court proceeding premised on recognized title loses its anchor; the settlement question dissolves into a general war of claims; and middle powers lose the operative precedent that borders are settled by law rather than force — the entire post-1945 border-stability norm absorbs the shock.
% FOUNDING_PROBLEM: Terminate the British Mandate over Palestine in the face of two armed national movements claiming exclusive sovereignty, escalating intercommunal war, and imminent British withdrawal — the UN needed a device converting an unmanageable possession fight into two legally constituted states before the territory collapsed into open war.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's existence is corroborated outside the beneficiary set by Mandate-era archival records, the UN Special Committee on Palestine's 1947 report with testimony from all parties, and subsequent historiography. That it remains live is attested by the continuing armed contest itself, documented by neutral monitors (UN OCHA casualty and displacement reporting) and recited in the ICJ's own advisory proceedings — no party disputes that the two-peoples-one-territory problem is unresolved; they dispute only which legitimacy source governs it.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).
:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45: from this reading's own lights the frame delivers real coordination (two recognized states remain structurally possible) while imposing bounded, identifiable costs — subordinated return claims, unlawful-status designations, line-divided communities — that fall on classes who never consented to the founding allocation. Suppression is 0.60: the frame's coercive apparatus (non-recognition, aid conditionality, court exposure, sanction risk) is real but veto-rationed; it binds weak and misaligned actors firmly and aligned ones hardly at all. Accessibility_collapse is 0.45: alternative readings and alternative arrangements (confederation, revised partitions) remain genuinely live, so understanding the frame does not close the option space. Resistance is 0.65: settlement expansion continues against the frame, patrons shield violations, and rival readings contest the title-source itself. Theater_ratio 0.38: recognition decisions, court opinions, and treaty access are materially consequential, but a growing share of the frame's activity is the recurring symbolic-resolution calendar whose output is restatement rather than effect. The temporal series run on ONE shared grid (1947/1956/1967/1980/1993/2002/2016/2025) with every tracked metric authored at every point; the visible 1993 dip in both extractiveness and theater is the Oslo phase — the cyclical pattern is crisis, negotiation, partial revival of the frame's function, then renewed accumulation as enforcement again fails, and the oscillation itself functions as intermittent reinforcement: each revival re-legitimates the frame precisely long enough for the next round of beyond-the-line consolidation. The suppression_requirement series is authored deliberately: it tracks enforcement-machinery intensity, which built steadily through 1980, dipped with the diplomatic turn of 1993, then climbed to a veto-capped plateau — the machinery matured without gaining decisive force. Receipt surface: gains demonstrably accrue to the great_power_patrons seat, which monetizes enforcement discretion directly as leverage; fixing_cost is prohibitive because removal would require universal renegotiation of recognition with systemic border-stability risk exceeding any regional benefit to whoever could fix it.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the administering organs' position the frame is functioning legal order — resolutions adopted, opinions issued, process maintained. From the settlement movement's position the same frame is pure adversarial designation: nothing it receives, everything it builds designated removable. From the refugee seat it is a price-setting machine that valued the loss while sealing the door. From the two recognized-state seats it is simultaneously foundation and cage — the source of legal existence and the ceiling on territorial ambition. The engine computes these per-seat classifications from the structural data (power, exit, role); the divergence between the administrator seat's orderly picture and the trapped payer seats' coercive picture is the measured signal, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Derivation from beneficiary/victim declarations plus exit options handles most seats: the settlement movement (declared victim, identity_locked) sits near the full-target end; refugees (declared victim, trapped, no state exercises their claims) likewise; the Council and Assembly (administrators whose authority the frame constitutes) sit near the beneficiary end; courts as analytical observers sit near symmetric. Two overrides are declared because the derivation would err: (1) power_atom moderate → d 0.55 — palestine_as_recognized_state is the canonical indirect-victim case: listed as beneficiary (recognition gains are real) but its trapped exit and the permanent gap between recognized borders and actual control mean it structurally bears the frame's largest unpaid costs; the derivation alone would place it implausibly near the beneficiary pole. (2) power_atom powerful → d 0.30 — both powerful agents are dual-positioned: green_line_israel nets recognition rents against settlement-restraint costs (symmetric-leaning, not the ~0.15 the beneficiary declaration suggests); great_power_patrons derive apparent beneficiary status from arbitrage-rich exit, but their gains come precisely from administering the extraction, and they carry credibility costs when stated rule and administered outcome diverge. Override granularity is per power atom; both powerful seats genuinely sit near 0.30, so no finer differentiation is asserted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — terminating a Mandate over two armed national movements in one territory — is live, and the arrangement retains real function: recognition still gates organizational membership, treaty access, court standing, and material aid, so the frame is neither a dead letter maintained by inertia nor performance alone (theater 0.38, below piton range). Nor is it pure extraction riding a fictitious coordination story: the two-state frame remains the only arrangement under which both national movements hold recognized statehood simultaneously. The R5 mismatch check runs clean: founding_problem_status=live paired with disappearance_verdict=world_rearranges produces no zombie flag — the arrangement persists because the problem persists, not because anyone profits from pretending otherwise. The classification discipline matters here in both directions: reading the frame as a pure coordination rope would erase the subordinated classes (refugees, line-divided communities) whose costs are structural; reading it as a snare would erase the genuine coordination achievement that keeps two-statehood structurally possible and that every seat still argues within. The tangled-rope claim holds both halves: coordinated through the same structure that extracts from identifiable seats, held together by enforcement that works unevenly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates only the partition reading of the territorial_legitimacy kernel — what structurally changes under each sibling reading, and where exactly is the disagreement located?',
    'Comparative authoring of the sibling files (territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading) against the same referent; the disagreement is located in the source-of-title axiom, which reassigns every seat''s position.',
    'Under the security-necessity reading, holdings beyond the Green Line gain conditional legitimacy and the payer set shifts toward populations behind barriers; under the indigenous-continuity reading the 1947-49 transfer becomes the central injury and both states'' title becomes contestable, raising ε sharply over the same referent. Sibling deltas are computed from the sibling files, never averaged into this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and the locus of disagreement.').

omega_variable(
    enforcement_materiality_gap,
    'Do recognition consequences bind behavior materially, or does the regime operate mainly as rhetorical condemnation?',
    'Quasi-experimental comparison of actor behavior around material recognition events — ICC jurisdiction acceptance, recognition waves, aid-conditionality episodes — measuring investment, construction rates, and treaty participation differentials.',
    'If materiality is low, the theater ratio understates decay and the arrangement drifts toward inertial performance; if high, the enforcement layer is the binding variable and the tangled-rope profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_materiality_gap, empirical, 'Whether legal recognition exerts material or merely expressive force on conduct.').

omega_variable(
    refugee_finality_coupling,
    'Is the subordination of refugee return claims a necessary structural feature of border finality, or separable via compensation and resettlement packages?',
    'Analysis of final-status negotiation history (Taba parameters, Geneva Initiative) testing whether return claims were ever priced independently of line finality, plus comparative evidence from other partition settlements with funded compensation tracks.',
    'If separable, the burden borne by the refugee seat is remediable within the arrangement and its effective position softens; if coupled, the seat''s burden is permanent and its computed classification hardens toward full-target extraction regardless of compliance elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_finality_coupling, conceptual, 'Whether refugee-claim costs are structurally coupled to line finality.').

omega_variable(
    founding_consent_deficit,
    'Does adoption by supermajority vote without the resident population''s consent ground durable legitimacy, or does the consent deficit persist as an unredeemed structural debt inside the arrangement?',
    'Conceptual analysis of validity theories (enacted-rule validity versus popular-sovereignty validity) plus a test of whether later consent instruments — the 1988 declaration, the Oslo mutual-recognition letters — retroactively cured the deficit.',
    'If curable, the founding act''s costs are amortizing and the arrangement trends toward ordinary coordination for descendant seats; if incurable, a permanent extraction residue attaches to the frame irrespective of current compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_consent_deficit, conceptual, 'Whether majority-vote imposition left an unredeemed consent debt at the frame''s foundation.').

omega_variable(
    great_power_selective_enforcement,
    'Is enforcement applied by rule or by patron alignment — does the veto structure convert the legal regime into an instrument of great-power preference?',
    'Cross-case coding of regime responses to comparable border-change episodes (Cyprus 1974, Crimea 2014, Western Sahara, East Jerusalem measures) controlling for violation type; measure the response differential by violator-patron alignment.',
    'Selective application splits per-seat experience: shielded actors feel little coercive force while targeted actors feel maximal force, so scalar metrics misdescribe every seat; confirms the powerful-atom override at 0.30 or forces recomputation of the administrator seats'' positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_selective_enforcement, empirical, 'Rule-based versus patron-aligned enforcement selectivity under the veto structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1947, territorial_legitimacy__partition_reading, theater_ratio, 1947, 0.12).
narrative_ontology:measurement_basis(terr_tr_t1947, observed).
narrative_ontology:measurement(terr_tr_t1956, territorial_legitimacy__partition_reading, theater_ratio, 1956, 0.16).
narrative_ontology:measurement_basis(terr_tr_t1956, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.21).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy__partition_reading, theater_ratio, 1980, 0.29).
narrative_ontology:measurement_basis(terr_tr_t1980, observed).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement_basis(terr_tr_t1993, observed).
narrative_ontology:measurement(terr_tr_t2002, territorial_legitimacy__partition_reading, theater_ratio, 2002, 0.31).
narrative_ontology:measurement_basis(terr_tr_t2002, observed).
narrative_ontology:measurement(terr_tr_t2016, territorial_legitimacy__partition_reading, theater_ratio, 2016, 0.36).
narrative_ontology:measurement_basis(terr_tr_t2016, observed).
narrative_ontology:measurement(terr_tr_t2025, territorial_legitimacy__partition_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(terr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1947, territorial_legitimacy__partition_reading, base_extractiveness, 1947, 0.22).
narrative_ontology:measurement_basis(terr_be_t1947, observed).
narrative_ontology:measurement(terr_be_t1956, territorial_legitimacy__partition_reading, base_extractiveness, 1956, 0.25).
narrative_ontology:measurement_basis(terr_be_t1956, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy__partition_reading, base_extractiveness, 1980, 0.37).
narrative_ontology:measurement_basis(terr_be_t1980, observed).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.33).
narrative_ontology:measurement_basis(terr_be_t1993, observed).
narrative_ontology:measurement(terr_be_t2002, territorial_legitimacy__partition_reading, base_extractiveness, 2002, 0.4).
narrative_ontology:measurement_basis(terr_be_t2002, observed).
narrative_ontology:measurement(terr_be_t2016, territorial_legitimacy__partition_reading, base_extractiveness, 2016, 0.43).
narrative_ontology:measurement_basis(terr_be_t2016, observed).
narrative_ontology:measurement(terr_be_t2025, territorial_legitimacy__partition_reading, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement_basis(terr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1947, territorial_legitimacy__partition_reading, suppression_requirement, 1947, 0.3).
narrative_ontology:measurement_basis(terr_su_t1947, observed).
narrative_ontology:measurement(terr_su_t1956, territorial_legitimacy__partition_reading, suppression_requirement, 1956, 0.36).
narrative_ontology:measurement_basis(terr_su_t1956, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.44).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy__partition_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement_basis(terr_su_t1980, observed).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.48).
narrative_ontology:measurement_basis(terr_su_t1993, observed).
narrative_ontology:measurement(terr_su_t2002, territorial_legitimacy__partition_reading, suppression_requirement, 2002, 0.56).
narrative_ontology:measurement_basis(terr_su_t2002, observed).
narrative_ontology:measurement(terr_su_t2016, territorial_legitimacy__partition_reading, suppression_requirement, 2016, 0.58).
narrative_ontology:measurement_basis(terr_su_t2016, observed).
narrative_ontology:measurement(terr_su_t2025, territorial_legitimacy__partition_reading, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(terr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'territorial legitimacy of Israel/Palestine' decomposes into three structurally distinct claims per the ε-invariance principle — they differ on the source-of-title axiom, and therefore on ε, victim sets, and failure modes, over the same physical referent. This file is the partition reading (title from international legal acts and recognition; settlements beyond the line illegitimate; two-statehood structurally possible). The partition reading is the doctrinal center of gravity: the security-necessity reading operates downstream of it (security claims must argue as deviations from the recognized-line default that recognition economics and court exposure establish), and the indigenous-continuity reading defines itself against it (rejecting the founding act's validity outright). Each file links to its siblings via affects_constraints; ε is authored independently in each and never averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, moderate, 0.55).
constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
