% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Vesting Constraint (Removal-Power Reading of Article II)
 *   domain: constitutional law/political theory/administrative law
 *
 * SUMMARY:
 *   Article II provides that 'the executive Power shall be vested in a
 *   President.' The unitary_executive_reading treats that vesting as
 *   exclusive and mandatory: all federal executive power is the President's,
 *   the removal of subordinate executive officers is incident to it, and
 *   Congress may not create agencies whose leadership is insulated from
 *   presidential removal and direction. Under this reading the independent
 *   regulatory state — the FTC, NLRB, SEC, and (contested within the
 *   reading's own tradition) the Federal Reserve — is structurally
 *   illegitimate. This story is ONE READING of the separation_of_powers_text
 *   kernel (see commentary.kernel_context); the formalist and functionalist
 *   readings are separate constraints with their own epsilon, victim sets,
 *   and classifications, linked through network.affects_constraints. The
 *   epsilon referent is the standing arrangement under contest — the
 *   constitutional order in which this reading's removal rule operates, with
 *   for-cause protections stripped and agencies subordinated — assessed by
 *   the reading's own lights: the reading weights the accountability function
 *   heavily and frames the power transfer as the text's own allocation rather
 *   than parasitic taking, which dampens epsilon relative to what a
 *   functionalist or congressional seat would author over the same referent,
 *   without erasing the transfer or its victims. The claimed type and the
 *   metrics are authored independently; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - presidency_executive_branch: agenda-setter and primary beneficiary (institutional/arbitrage) — asserts and enforces the removal rule, collects consolidated control of the administrative state
 *   - presidential_loyalist_appointees: secondary beneficiary (moderate/mobile) — occupy the offices the consolidation opens; careers ride the patronage network
 *   - independent_agency_commissioners: primary target (moderate/trapped) — statutory for-cause protection nullified; every executive office remains inside the rule's reach
 *   - independent_regulatory_agencies: target institutions (organized/constrained) — FTC, NLRB, SEC, Fed; converted from insulated expert bodies to subordinate executive components
 *   - congress_structural_interest: target and co-agenda-setter (institutional/constrained) — wrote the insulation statutes the reading voids; remedies are collective and slow
 *   - federal_judiciary: target and enforcement venue (institutional/constrained) — fixes the rule's doctrinal reach while its precedent line and agency buffer erode
 *   - agency_service_constituencies: excluded (powerless/trapped) — bear politicized enforcement downstream with no seat in the contest
 *   - administrative_law_academy: analytical observer — maps the contest, collects no rents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.62).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.65).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Vesting Constraint (Removal-Power Reading of Article II)").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional law/political theory/administrative law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, 'eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea').
narrative_ontology:cs_kernel_codification('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', fixed_text).
narrative_ontology:cs_authority_grounding('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', lineage).
narrative_ontology:cs_interpretation_layer_present('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea').
narrative_ontology:cs_reading_relation('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', foundational, article_ii_vesting_exclusive_and_mandatory).
narrative_ontology:cs_axiom_status(article_ii_vesting_exclusive_and_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', article_ii_vesting_exclusive_and_mandatory, conventional).
narrative_ontology:cs_axiom('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', foundational, removal_power_incident_to_vested_executive_power).
narrative_ontology:cs_axiom_status(removal_power_incident_to_vested_executive_power, holdable).
narrative_ontology:cs_axiom_grounding('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', removal_power_incident_to_vested_executive_power, conventional).
narrative_ontology:cs_reference_frame('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', founding_era_plenary_vesting).
narrative_ontology:cs_drift_state('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', contemporary_removal_litigation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('eccdf7f7-dcf3-44e6-b5c9-87054ecff9ea', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, presidency_executive_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, presidential_loyalist_appointees).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agency_commissioners).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_regulatory_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress_structural_interest).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_judiciary).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, article_ii_exclusive_vesting).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, decision_of_1789_removal_precedent).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, unitary_accountability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces plenary removal and direction authority over every executive officer: it removes commissioners protected by statute, redirects agency enforcement priorities, installs acting leadership, and litigates to extend the removal rule through the Solicitor General, with OLC opinions supplying the doctrinal record. It collects the consolidated control this produces. Its options are maximal: it can litigate, restructure, remove, and reshape doctrine, and it bears none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, presidency_executive_branch, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, presidency_executive_branch, beneficiary).

% Occupy the acting and confirmed positions that consolidated appointment and removal control opens across the agencies. Their offices, discretion, and careers flow from the presidency's expanded control; when one position ends, the network that placed them can place them again.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, presidential_loyalist_appointees, beneficiary,
    moderate, immediate, mobile, national).

% Hold statutorily protected seats on multimember agencies — staggered fixed terms, removable only for cause. The reading nullifies that protection: they serve at presidential pleasure. They cannot exit the rule's reach, because every executive-branch office they might hold falls under the same vesting clause, and resignation forfeits the office entirely.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agency_commissioners, payer,
    moderate, biographical, trapped, national).

% Are institutions — the FTC, NLRB, SEC, and (contested within the reading's own tradition) the Federal Reserve — built on statutory insulation so that expert enforcement would run at arm's length from political direction. The reading converts them into subordinate components of the executive branch. They can litigate, restructure internally, and seek statutory amendment, but they cannot leave the branch they are part of.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_regulatory_agencies, payer,
    organized, generational, constrained, national).

% Writes the statutes that create agencies and insulate their leadership, and relies on that insulation to have its laws executed by bodies it can reach through oversight but the President cannot reach through removal. The reading voids the insulation tool. Its remaining remedies — constitutional amendment, impeachment, confirmation leverage, appropriations — are collective, supermajoritarian, and slow, and each Congress inherits the arrangement afresh from the one before.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress_structural_interest, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, congress_structural_interest, agenda_setter).

% Decides the removal and delegation cases that fix the rule's doctrinal reach, and its current majority serves as the rule's enforcement venue. The institution around that majority loses on both flanks: its Humphrey's Executor precedent line is overruled case by case, and the independent agencies that absorbed confrontation between the presidency and the regulatory state are dissolved into direct presidential control, leaving the Article III courts as the only remaining formal check.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, federal_judiciary, agenda_setter).

% Are the workers, consumers, and market participants who depend on what insulated agencies enforce — labor protections, consumer protection, monetary stability. They experience politicized or redirected enforcement only as downstream effects: cases not brought, rules unenforced, policy swinging with each administration. They have no seat in the removal-power contest that decides who controls the agencies, and they cannot exit the jurisdiction of the enforcement they depend on.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, agency_service_constituencies, excluded,
    powerless, biographical, trapped, national).

% Produces the competing readings, maps the doctrinal movement, and testifies as experts in the removal litigation. It collects no rents from the rule and bears no removal risk; its stake is interpretive.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, administrative_law_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, presidency_executive_branch).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates execution of federal law in a single constitutionally accountable officer: one elected President answers for every act of the executive apparatus, giving voters a single target for reward and blame and giving administration an energy and decisiveness that a fragmented, self-protective bureaucracy cannot supply.
% TRANSFER_FUNCTION: Moves structural power over the administrative state — removal authority, policy direction, enforcement discretion, and the ability to install or dismiss leadership — from Congress's statutorily insulated designs and from independent agency heads to the presidency; functionally it also strips the judiciary of the independent-agency buffer that stood between it and direct confrontation with executive power.
% ABSENT_VOICES: The constituencies that independent regulation serves — unionized workers before the NLRB, consumers before the FTC, market participants relying on Federal Reserve independence — have no seat in the removal-power contest; they experience politicized enforcement only as its downstream effects. The agencies themselves appear only as litigants defending their own existence, never as designers of the settlement being displaced.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight — if the exclusive-vesting reading were abandoned — the administrative state would reorganize within a few years: Congress would re-legislate for-cause protections, removed commissioners would litigate reinstatement, agencies would reconstitute their insulated structures, and the removal-litigation wave would end with the pre-Seila Law settlement substantially restored. The presidency would lose its consolidated control; no other arrangement depends on the constraint's persistence.
% FOUNDING_PROBLEM: The Articles of Confederation left execution to committees with no unity and no accountability; the Framers' remedy was a single elected President in whom executive power would vest, so that administration would be energetic and its failures attributable. The First Congress's removal debates (the Decision of 1789) extended the design to the removal of subordinate officers.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the benefiting parties: constitutional historians and administrative-law scholars across the functionalist and formalist camps concede that the vesting clause was designed to create a single accountable executive — Federalist 70's energy-in-execution argument is invoked by every side of the dispute. What no seat outside the unitary-executive coalition corroborates is the reading's answer to that problem — the exclusivity claim that the design forecloses congressional insulation — which functionalist scholarship and the Humphrey's Executor line expressly deny. The problem is corroborated; the solution is contested by every non-benefiting seat.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62: the removal rule transfers real, concentrated structural power from identifiable parties — commissioners' tenure, Congress's design authority, the judiciary's agency buffer — to the presidency. The value is authored from the reading's own lights, which credit the accountability coordination the consolidation supplies; a functionalist seat over the same referent would author 0.8+. Suppression is 0.65 and rising: the constraint's persistence now requires an active enforcement machinery — the Seila Law removal rule, the Jarkesy curtailment of agency adjudication, the 2024-2025 removal wave against multimember commissions, OLC opinion support — that suppresses both agency independence claims and the functionalist alternative in doctrine. Suppression is authored as a raw structural property, unscaled by scope or directionality; only extractiveness is engine-scaled. Theater is 0.18 and falling: for most of the interval the principle existed as doctrinal assertion without operational effect (theater near 0.6 in the Humphrey's Executor era — law-review articles, dissenting opinions, OLC memoranda); as enforcement became real, performance gave way to actual removals and restructuring. Accessibility_collapse is 0.5: alternatives persist — Humphrey's Executor nominally survives for multimember agencies, the Fed carve-out is live inside the reading's own tradition, and Congress retains slow remedies — so the alternative does not fully collapse on understanding the constraint. Resistance is 0.7: the constraint meets the most sustained doctrinal and political resistance in American public law. The measurement series share one grid (t=0 to t=90, years since Humphrey's Executor in 1935); all three tracked metrics are authored at every shared time point, and the suppression_requirement series is included because the story specifically tracks enforcement-capacity change — the machinery was judicially dismantled in 1935 and rebuilt from 1983 forward.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat and the payer seats compute different types from the same structure. From the presidency's seat the constraint is constitutional fidelity: the text vests executive power, accountability requires control, and the 'victims' are simply parties holding power the text never gave them — the seat experiences coordination, not extraction. From the commissioners' and Congress's seats the same rule operates as expropriation: statutory protections purchased through the legislative process are voided by a reading those seats never accepted. Congress and the judiciary share an institutional power level but diverge on constraint-specific factors: Congress loses its design tool and can respond only through collective, supermajoritarian mechanisms; the judiciary is simultaneously the constraint's enforcement venue (its majority decides the cases) and a functional loser (its precedent line is overruled and its buffer dissolved) — a dual position the secondary_role records. The coalition possibility for the payer side exists structurally — Congress, the agencies, and the service constituencies could combine — but is blunted by supermajority requirements, presidential veto, and the constituencies' absence from the contest, which is why measured resistance stays high without becoming effective. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency sits at the beneficiary end (d near 0): it collects the transfer and controls enforcement, with arbitrage-grade options — it can litigate, restructure, remove, and reshape doctrine at will, and no part of the arrangement costs it anything. Loyalist appointees are secondary beneficiaries (low d): they gain offices and careers but do not run the constraint. Commissioners and the agencies sit near the target end (d near 1): they bear the removal risk and the loss of institutional insulation, and their exit is trapped (commissioners — every alternative office is under the same vesting clause) or constrained (agencies cannot leave the branch they are part of). Congress sits high-d as payer despite its agenda-setter secondary role: the statutes it writes are the objects the constraint nullifies, and its remedies are too slow to constitute exit. The judiciary is the ambiguous seat — its enforcement role dampens d while its functional losses raise it; the structural data leave it mid-to-high d, and no directionality override is authored because the derivation from its dual role and constrained exit is defensible as-is.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification keeps two errors apart. Reading the constraint as pure coordination would launder the transfer: the accountability function is real, but it rides on identifiable victims whose protections are voided by active enforcement rather than by consent, and the gains concentrate in one seat. Reading it as pure extraction would erase the coordination function: a single accountable executive solves a genuine collective-action problem that predates the modern administrative state and that every seat in the contest concedes was the founding design's purpose. The founding-problem interview corroborates the coordination side — the founding problem (energetic, accountable execution) is live and corroborated from outside the benefiting parties — so the mismatch consumer finds no zombie flag: this is a constraint whose coordination function is current, not a mandate outliving its function. The classification would drift toward pure extraction if the accountability function proved to be cover — if consolidation produced less accountable administration (politicized enforcement, loyalty screening, redirected cases) rather than more — which the revival_trajectory omega tracks alongside the rising suppression series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the separation_of_powers_text kernel — the unitary_executive_reading. What would change structurally if a sibling reading (formalist_reading, functionalist_reading) were the operative constraint instead, and where exactly is the disagreement located?',
    'Doctrinal settlement: if the Supreme Court adopts exclusivity in full (overruling Humphrey''s Executor for multimember agencies), this reading becomes the operative constraint and the functionalist sibling is foreclosed in doctrine; if the Court reaffirms insulation, the functionalist reading governs and this story''s victim set dissolves.',
    'Determines which of the three kernel stories describes the live constitutional order. A functionalist settlement empties this constraint''s victim set (insulated agencies become constitutional) and its classification collapses toward pure coordination; a formalist settlement shifts the contested terrain from removal to delegation and changes the beneficiary structure. The three stories are linked constraints, not alternative descriptions of one thing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which kernel reading this story instantiates, what siblings would change, and where the readings disagree (exclusivity vs. allocativeness of the vesting clause).').

omega_variable(
    federal_reserve_carve_out,
    'Does the reading''s removal rule reach the Federal Reserve''s monetary-policy functions? The structural delta names the Fed among the victims, but Seila Law''s dicta and much unitary scholarship carve the Fed out; the reading''s own tradition is split on this.',
    'Pending and future litigation over Federal Reserve governor removal protection: a Court ruling applying the Seila Law rule to the Fed, or an express doctrinal carve-out, resolves it.',
    'If the Fed is carved out, the victim set narrows, the constraint''s scope contracts, and effective extraction drops; if not, monetary policy enters the removal calculus and extraction rises sharply with macroeconomic consequences attached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_reserve_carve_out, empirical, 'Whether the Fed sits inside or outside the removal rule this reading enforces.').

omega_variable(
    textual_compulsion_vs_construction,
    'Is the exclusivity premise a discovery about the text''s fixed public meaning, or a constructed doctrine whose current force comes from a coordinated judicial-political coalition? Proponents present the vesting clause as textually compelled; opponents read the same clause as allocative.',
    'Originalist methodology applied to the founding record: if the public meaning of ''the executive Power'' in 1788 was exclusive, the premise is discovery; if the phrase was understood as allocative — as the First Congress''s own statutory practice and the rapid emergence of protected officers suggest — the premise is construction.',
    'If construction, the constraint loses its claimed textual inevitability and its classification drifts toward pure extraction defended by coalition power; if discovery, the extraction measured here is the Constitution''s own price of accountability and the coordination reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_compulsion_vs_construction, conceptual, 'The naturalness ambiguity at the reading''s core: fixed constitutional fact vs. constructed doctrine.').

omega_variable(
    revival_trajectory,
    'The measurement series shows the constraint dormant for roughly half the interval (judicially repudiated in 1935), then reviving with accelerating extraction from 1983 forward. Does the revival run to completion — absolute removal power, Humphrey''s Executor fully overruled, the Fed included — or stall at partial revival?',
    'Track the pending removal-litigation wave and any express overruling of Humphrey''s Executor; watch whether the Seila Law rule is extended to multimember agencies or held at single-director agencies.',
    'Full revival pushes the constraint toward snare-flavored consolidation (suppression and extraction rising past 0.75, alternatives fully collapsed); a stall freezes it as a hybrid with a permanently contested core and persisting alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_trajectory, empirical, 'Whether the enforcement ratchet completes or the constraint stabilizes as a contested hybrid.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__unitary_executive_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(sepa_tr_t15, separation_of_powers_text__unitary_executive_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__unitary_executive_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(sepa_tr_t45, separation_of_powers_text__unitary_executive_reading, theater_ratio, 45, 0.52).
narrative_ontology:measurement(sepa_tr_t60, separation_of_powers_text__unitary_executive_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(sepa_tr_t75, separation_of_powers_text__unitary_executive_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(sepa_tr_t85, separation_of_powers_text__unitary_executive_reading, theater_ratio, 85, 0.25).
narrative_ontology:measurement(sepa_tr_t90, separation_of_powers_text__unitary_executive_reading, theater_ratio, 90, 0.18).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sepa_be_t15, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(sepa_be_t45, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 45, 0.25).
narrative_ontology:measurement(sepa_be_t60, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(sepa_be_t75, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement(sepa_be_t85, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 85, 0.58).
narrative_ontology:measurement(sepa_be_t90, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 90, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(sepa_su_t15, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(sepa_su_t45, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 45, 0.22).
narrative_ontology:measurement(sepa_su_t60, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(sepa_su_t75, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement(sepa_su_t85, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 85, 0.55).
narrative_ontology:measurement(sepa_su_t90, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 90, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'separation of powers' covers at least three structurally distinct constraints (per the epsilon-invariance principle): the unitary-executive reading (this story — removal power and agency insulation), the formalist reading (nondelegation and impermeable boundaries), and the functionalist reading (flexible delegation; insulated agencies are constitutional). Their epsilon values differ because their victim sets differ: this reading strips protections from agencies, Congress, and the judiciary's buffer; the formalist strips the delegated legislative function; the functionalist strips no one. This story links to both siblings: the formalist reading is its closest ally — a single originalist framework comfortably holds both no-delegation and exclusive vesting (coexists_with) — while the functionalist reading is its direct doctrinal opposite, since exclusive vesting and permissible insulation of executive officers cannot both hold in one framework (forecloses).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
