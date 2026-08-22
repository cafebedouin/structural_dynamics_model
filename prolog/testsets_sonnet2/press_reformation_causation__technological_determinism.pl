% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Deterministic Cause of the Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism reading of the
 *   press_reformation_causation kernel: the claim that Gutenberg's press was
 *   an exogenous, mountain-like technological development whose replication
 *   capacity made effective censorship structurally impossible, and that
 *   vernacular scripture distribution — and thus the Reformation's spread —
 *   followed as a near-inevitable downstream consequence. On this reading,
 *   the Church's suppression efforts (indices, licensing, burnings) were
 *   structurally futile against a technology that had crossed a replication
 *   threshold; reformers are cast as beneficiaries of an exogenous capacity
 *   shift rather than as strategic authors of it. This is the classic
 *   Eisenstein-style 'printing revolution' thesis in its strong form. Two
 *   sibling constraints exist for the same kernel and are NOT part of this
 *   story: strategic_deployment (reformers/printers as purposive agents
 *   exploiting neutral technology) and mutual_shaping (technology and agency
 *   co-evolving). Each sibling has its own ε and its own stakeholder
 *   structure; they are linked here only via network edges.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Primary beneficiary (organized/mobile) — gain rapid, hard-to-suppress distribution of vernacular theology
 *   - vernacular_printers: Secondary beneficiary (moderate/mobile) — commercial beneficiaries of demand for reformist and vernacular texts
 *   - catholic_ecclesiastical_authorities: Primary structurally-disempowered actor (institutional/constrained) — attempt suppression that this reading holds as technologically futile
 *   - lay_vernacular_readers: Downstream population affected — gain access to scripture and pamphlets in their own language
 *   - modern_technological_determinist_historians: Analytical beneficiary (analytical/arbitrage) — the reading vindicates their explanatory framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.28).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.15).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Deterministic Cause of the Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '6173e8eb-453d-4eb5-9e3a-bee8c75c5138').
narrative_ontology:cs_kernel_codification('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', distributed).
narrative_ontology:cs_authority_grounding('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', distributed).
narrative_ontology:cs_reading_relation('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', foundational, technology_as_exogenous_unstoppable_force).
narrative_ontology:cs_axiom_status(technology_as_exogenous_unstoppable_force, holdable).
narrative_ontology:cs_axiom_grounding('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', technology_as_exogenous_unstoppable_force, empirically_contingent).
narrative_ontology:cs_axiom('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', secondary, reformer_agency_causally_secondary_to_technological_capacity).
narrative_ontology:cs_axiom_status(reformer_agency_causally_secondary_to_technological_capacity, holdable).
narrative_ontology:cs_axiom_grounding('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', reformer_agency_causally_secondary_to_technological_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', eisenstein_printing_revolution_thesis).
narrative_ontology:cs_drift_state('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', post_johns_pettegree_revisionism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6173e8eb-453d-4eb5-9e3a-bee8c75c5138', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, modern_technological_determinist_historians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, lay_vernacular_readers).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technology_as_exogenous_historical_prime_mover).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, censorship_inevitably_fails_against_replicable_media).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose vernacular theological tracts and translated scripture that spread through print networks faster than ecclesiastical authorities can identify and suppress sources. On this reading they did not need to out-maneuver censorship tactically — the press's replication rate structurally outpaced any feasible suppression apparatus, so their message reached lay audiences as an approximately automatic consequence of the technology's capacity.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Operate presses across fragmented European jurisdictions; when suppressed in one city, relocate operations to a neighboring polity with looser licensing. Profit from strong demand for vernacular religious material. Their mobility across jurisdictional lines is itself part of why the determinist reading treats suppression as structurally impossible — no single authority controlled enough territory to close every press.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_printers, beneficiary,
    moderate, biographical, mobile, regional).

% Attempt to control the spread of heterodox and vernacular texts through licensing regimes, the Index of Prohibited Books, book burnings, and printer prosecutions. On this reading, their institutional machinery is structurally overwhelmed: press replication capacity exceeds any feasible enforcement rate across fragmented European jurisdictions, so intensifying suppression effort yields diminishing real effect even as the apparatus itself grows.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_ecclesiastical_authorities, agenda_setter,
    institutional, civilizational, constrained, continental).

% Gain direct access to scripture and religious pamphlets in their own language for the first time at scale, without needing clerical mediation. Their access follows from the technology's cost collapse and replication rate, not from any negotiated concession by ecclesiastical authorities.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, lay_vernacular_readers, beneficiary,
    powerless, biographical, constrained, regional).

% Advance and defend the printing-revolution thesis (in the Eisenstein tradition) as the dominant explanatory frame for the Reformation's rapid spread. Their scholarly authority and the clean causal narrative both depend on the technology-as-mountain framing being accepted as historically accurate rather than as one contested reading among several.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, modern_technological_determinist_historians, beneficiary,
    analytical, civilizational, analytical, global).

% Scholars in the tradition of Johns and Pettegree who argue print culture's effects were locally contingent, contested, and shaped by strategic human choices rather than technologically predetermined; their revisionist account is not part of this story's own framing and is structurally excluded from the determinism reading's self-understanding, even though it directly challenges this reading's foundational axiom.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, revisionist_book_historians, excluded,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__technological_determinism, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the strict sense — this reading does not model a coordination problem being solved by an agreed arrangement; it models a technological capacity shift (movable-type printing) that structurally altered the feasibility of information control, independent of any coordinating agreement among parties.
% TRANSFER_FUNCTION: No deliberate transfer mechanism is claimed; the reading holds that reach and influence redistributed from centralized ecclesiastical gatekeepers toward reformist authors, printers, and lay readers as an emergent consequence of replication economics, not as an intended transfer.
% ABSENT_VOICES: Revisionist book historians (Johns, Pettegree, and others who stress contingency and strategic agency) are structurally excluded from this reading's own self-understanding — they would object that 'inevitability' overstates the case and that effective, if partial, censorship regimes did persist for extended periods in some polities. They exist in the historiographical literature but not within this reading's framing.
% DISAPPEARANCE_RATIONALE: If the technological-determinism reading itself 'disappeared' (were universally rejected as an explanatory frame), the underlying historical events (Reformation, print spread) would not change, but the causal narrative attributing them to a mountain-like technological threshold would lose scholarly and popular currency — shifting weight toward the strategic_deployment or mutual_shaping siblings. Whether that shift 'rearranges the world' depends on how much of modern Protestant institutional self-understanding and media-studies pedagogy is actually built on the strong determinist narrative versus merely citing it loosely; parties dispute how load-bearing the narrative is.
% FOUNDING_PROBLEM: The reading was constructed to explain why Reformation ideas spread with unprecedented speed and geographic breadth compared to earlier heterodox movements (e.g., Lollardy, Hussite reform) that ecclesiastical authorities successfully contained — the founding problem is explaining differential suppression success across eras.
% FOUNDING_PROBLEM_CORROBORATION: Elizabeth Eisenstein's own scholarship (the reading's primary intellectual source) attests the problem and endorses the strong-determinist answer. Outside the reading's own tradition, book historians Adrian Johns and Andrew Pettegree corroborate that the founding problem (why did this heterodoxy spread where others didn't) is real and worth explaining, but dispute the determinist answer, arguing print's effects were locally contingent and required active strategic exploitation by named actors — corroboration of the problem exists outside the beneficiary set, but corroboration of THIS reading's specific answer does not.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because this reading treats the causal mechanism itself as a natural-law-like technological threshold rather than as an extraction of value from a specific victim group — there is no clear payer in the determinist frame, only differential winners (reformers, printers) and a structurally overwhelmed loser (ecclesiastical censors) who is not so much extracted-from as bypassed. Suppression is authored low (0.15) because determinism as a reading claims censorship COULDN'T work, not that it was actively and successfully coercive — the whole point of this reading is that coercive suppression structurally failed. Theater ratio rises across the interval (0.20 to 0.44) reflecting that Church suppression apparatus (indices, licensing bureaucracies) persisted and even intensified institutionally even as, on this reading's own terms, it became increasingly performative against a technology that had already won. Accessibility collapse is authored high (0.72) because the determinist reading's core claim is precisely that alternatives to eventual vernacular dissemination had collapsed once the press reached critical replication capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical authority seat, the story looks like a losing rope — coordination effort undermined by a hostile new capacity, with resistance measured in real institutional cost. From the reformer/printer seat, the same structure reads as a straightforward beneficiary windfall — an exogenous mountain shifted the payoff structure in their favor with minimal cost to them. The determinism reading itself does not adjudicate this gap; it structurally favors the mountain framing by declaring the technology exogenous and unstoppable, which is exactly why this reading requires the FSM omega — a 'mountain' that identifiable parties benefit from is the signature the corpus is built to flag.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers and vernacular printers are declared beneficiaries because the determinist frame casts them as downstream recipients of an exogenous capacity shift — they did not need to strategize around censorship because the technology structurally defeated it for them. This is a low-d, subsidized position. Ecclesiastical authorities are not declared victims in the schema sense (no clean extraction transfer) but are the structurally disempowered actor whose suppression capacity is rendered futile by the mountain's operation — this is why no victims array is authored; a mountain reading has no payer, only a bypassed incumbent.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandatrophy risk is specific: if the press's censorship-defeating capacity is treated as a permanent, ahistorical mountain, it obscures the possibility that later, more sophisticated information-control technologies (state licensing regimes, later print monopolies, and eventually digital surveillance) reconstituted effective censorship using different mechanisms — meaning the 'inevitability' claim may only have held for a specific 150-year window and should not be treated as a timeless natural law about media and censorship in general. Classifying this as a bounded mountain (with FSM omega flagging beneficiary presence) rather than an unqualified eternal truth prevents this reading from being used to overclaim inevitability in unrelated later media contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_narrative,
    'Is the claim that the press made censorship ''impossible'' a genuine natural-law-like technological threshold, or a retrospectively constructed narrative that benefits reformers'' theological legitimacy and modern determinist historiography by making the Reformation look inevitable rather than contingent on strategic choices?',
    'Comparative case analysis: examine polities (e.g., parts of Spain, Italy) where print censorship was substantially effective for extended periods despite press availability. If effective suppression was achievable with sufficient state/church coordination, the ''impossibility'' claim is a constructed overstatement, not a mountain.',
    'If censorship was in fact often effective, this reading''s core mountain claim collapses and the constraint reclassifies toward a contested/constructed narrative that serves identifiable beneficiaries (reformers, determinist historians) — likely a false-summit pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_narrative, conceptual, 'Whether press-driven censorship-impossibility is a genuine technological floor or a beneficiary-serving historiographical construction.').

omega_variable(
    committer_kernel_disaggregation,
    'This story is one of three readings of the press_reformation_causation kernel (technological_determinism, strategic_deployment, mutual_shaping). Which reading best captures the actual causal structure, and is the determinism reading a defensible extreme case or an oversimplification that the other two readings correct?',
    'Cross-reading comparison using the network edges to strategic_deployment and mutual_shaping constraints; historiographical consensus-tracking (has scholarly opinion moved away from strict determinism toward mutual_shaping accounts, e.g. in work following Eisenstein vs. her critics like Johns and Pettegree?).',
    'If mutual_shaping has become the dominant scholarly reading, this determinism reading should be understood as a minority/superseded position within the profession, even though it remains a live popular narrative — affecting how much weight its ''vindicated propositions'' should carry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disaggregation, conceptual, 'Committer-structure note: this reading''s relative standing among the three kernel readings of press-Reformation causation.').

omega_variable(
    reader_beneficiary_ambiguity,
    'Do reformers and vernacular printers actually benefit from the technology being framed as deterministic (removing moral/strategic responsibility from their choices), or is the beneficiary structure instead located entirely in the modern historiographical guild that finds determinism a cleaner explanatory frame?',
    'Examine whether 16th-century reformers themselves argued in providential/determinist terms (they largely did, theologically) versus whether this was a retrospective historian''s frame — check primary reformer rhetoric (e.g., Luther''s own comments on printing) against modern historiographical claims.',
    'If reformers'' own self-understanding was providential/deterministic, the beneficiary declaration is well-grounded structurally, not just an artifact of later historiography.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reader_beneficiary_ambiguity, empirical, 'Whether the beneficiary declaration reflects contemporary actor self-understanding or only later historiographical convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.2).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causation__technological_determinism, theater_ratio, 1490, 0.28).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__technological_determinism, theater_ratio, 1517, 0.35).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causation__technological_determinism, theater_ratio, 1560, 0.4).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__technological_determinism, theater_ratio, 1600, 0.44).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__technological_determinism, theater_ratio, 1650, 0.42).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causation__technological_determinism, base_extractiveness, 1490, 0.2).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__technological_determinism, base_extractiveness, 1517, 0.24).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causation__technological_determinism, base_extractiveness, 1560, 0.28).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__technological_determinism, base_extractiveness, 1600, 0.3).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__technological_determinism, base_extractiveness, 1650, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__technological_determinism, 0.05).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'the printing press caused the Reformation' per the ε-invariance principle. Each reading (technological_determinism, strategic_deployment, mutual_shaping) is authored as a separate constraint with its own ε, beneficiary structure, and claimed_type, because measuring causal responsibility differently (exogenous technology vs. strategic agency vs. co-evolution) yields structurally different extraction and suppression profiles. All three are linked via network edges; none subsumes the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
