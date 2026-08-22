% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Print-Cost Collapse as Reformation Cause (Technological Determinism Reading)
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   This story instantiates the technological-determinism reading of the
 *   contested kernel 'did print cause the Reformation': the claim that once
 *   movable-type printing reduced reproduction costs below the scribal
 *   threshold, mass vernacular scripture distribution became physically
 *   irresistible and the Reformation inevitable — reformers as downstream
 *   adapters riding a cost curve they did not create. The standing
 *   arrangement under contest is the early-modern print economy of religious
 *   text, c.1450-1555. The reading assesses epsilon by its own lights: the
 *   arrangement's economic essence derives from production cost reduction, so
 *   extractiveness sits near the coordination floor — the press subsidized
 *   readers and printers rather than extracting from anyone. The claim is
 *   mountain (emerges_naturally true: the cost curve is asserted to be a
 *   structural feature of the technology, not a human choice). The structural
 *   data — four declared beneficiary groups, two declared cost-bearing
 *   groups, a century of organized resistance — is authored independently of
 *   the claim, and where the engine's per-seat computation diverges from the
 *   mountain claim, that divergence is the false-summit measurement this
 *   story exists to take. KEY AGENTS (by structural relationship): -
 *   printers_publishers: primary beneficiary (organized/constrained) -
 *   reformist_theologians: secondary beneficiary (organized/identity_locked)
 *   - vernacular_reading_laity: diffuse beneficiary (powerless/mobile) -
 *   territorial_rulers: indirect beneficiary (institutional/constrained) -
 *   catholic_church_hierarchy: primary cost-bearer (institutional/trapped) -
 *   scribal_scriptoria_communities: displacement cost-bearer
 *   (powerless/constrained) - illiterate_rural_laity: excluded voice
 *   (powerless/trapped) - historians_of_print: analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - printers_publishers: Primary beneficiary (organized/constrained) — operates the presses, bears fixed costs and censorship risk, collects the mass text market the cost curve opened
 *   - reformist_theologians: Secondary beneficiary (organized/identity_locked) — downstream adapter whose movement the channel carried; professional and confessional identity fused with the cause
 *   - vernacular_reading_laity: Diffuse beneficiary (powerless/mobile) — receives scripture at prices scribal production never reached; participation voluntary
 *   - territorial_rulers: Indirect beneficiary (institutional/constrained) — collects the political rearrangement the channel enabled; committed past easy reversal
 *   - catholic_church_hierarchy: Primary cost-bearer (institutional/trapped) — loses a millennium-old information monopoly; its resistance defines the constraint's resistance profile
 *   - scribal_scriptoria_communities: Displacement cost-bearer (powerless/constrained) — the livelihood the cost curve erased, with no organized defense
 *   - illiterate_rural_laity: Excluded voice (powerless/trapped) — outside the print-public sphere whose creation rearranged authority over them
 *   - historians_of_print: Analytical observer (analytical/analytical) — reconstructs cost series and diffusion paths; the seat from which the causal dispute is adjudicated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.08).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.12).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Print-Cost Collapse as Reformation Cause (Technological Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history of technology / religious history / media studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, 'e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea').
narrative_ontology:cs_kernel_codification('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', distributed).
narrative_ontology:cs_authority_grounding('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', diffuse_epistemic).
narrative_ontology:cs_reading_relation('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', technology_reformation_causality__co_constitution_reading, forecloses).
narrative_ontology:cs_axiom('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', foundational, print_cost_collapse_sufficient_for_reformation).
narrative_ontology:cs_axiom_status(print_cost_collapse_sufficient_for_reformation, holdable).
narrative_ontology:cs_axiom_grounding('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', print_cost_collapse_sufficient_for_reformation, empirically_contingent).
narrative_ontology:cs_axiom('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', secondary, reformers_as_downstream_adapters).
narrative_ontology:cs_axiom_status(reformers_as_downstream_adapters, holdable).
narrative_ontology:cs_axiom_grounding('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', reformers_as_downstream_adapters, empirically_contingent).
narrative_ontology:cs_reference_frame('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', print_technological_sufficiency).
narrative_ontology:cs_drift_state('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', post_revisionist_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5981e52-56c5-4f49-b9ca-a6a7ae9cf6ea', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, printers_publishers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, reformist_theologians).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_reading_laity).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, territorial_rulers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, scribal_scriptoria_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, printers_publishers).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, print_revolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate workshops of presses and type; bear heavy fixed costs and the censorship risk of whatever they print — several were tried, exiled, or executed for reformist output. Collect the profits of a market where a pamphlet that took a scriptorium months now sells in thousands within weeks. Exit means writing off presses, type, and trained crews, so most stay in the trade and ride the cost curve.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, printers_publishers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__technological_determinism_reading, printers_publishers, payer).

% Produce the argument and the text: vernacular sermons, pamphlets, translated scripture. Before print, a reformer's reach was the pulpit and the slow manuscript; with print, one tract reaches every press town faster than any edict can answer it. Their standing is fused with the cause — recantation would end them as authorities — so the channel and the identity grow together.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformist_theologians, beneficiary,
    organized, generational, identity_locked, continental).

% Buy or hear read the cheap scripture and pamphlets now priced within household reach, gaining direct access to texts previously mediated entirely through clergy. Participation is voluntary and the cost is the price of a booklet; they can and do simply stop reading.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_reading_laity, beneficiary,
    powerless, biographical, mobile, continental).

% Decide which confession their territory adopts and collect the political rearrangement the new channel enables: confiscated church property, independence from Rome, a printing industry that serves administration. Once committed, reversal means civil war, so their options narrow with every year of commitment.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, territorial_rulers, beneficiary,
    institutional, generational, constrained, continental).

% Holds a millennium-old monopoly on scriptural reproduction and interpretation that the cost curve dissolves. Bans, burnings, and index machinery consume resources and fail to keep pace; conceding vernacular scripture would concede the doctrinal ground, so the hierarchy cannot exit its own position. It also prints heavily for its own purposes, which does not slow the loss of control.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Copy manuscripts for a living in a market where the printed book undercuts them by an order of magnitude. Some move into print shops as compositors and correctors; the rest watch commissions vanish. Their skill is the thing being displaced, and as fragmented guilds they mount no organized defense.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, scribal_scriptoria_communities, payer,
    powerless, biographical, constrained, continental).

% Remain outside the print-public sphere entirely: they cannot read what the cost curve made cheap and stay dependent on oral mediation, now delivered by whichever confession controls their territory's pulpit. The rearrangement of religious authority over them proceeds without their voice.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, illiterate_rural_laity, excluded,
    powerless, biographical, trapped, continental).

% Reconstruct the cost series, print runs, and diffusion paths; assess whether the causal weight sits in the technology, the actors, or their interaction. No stake in the confessional outcome; their disputes structure how every other seat's experience is remembered.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, historians_of_print, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__technological_determinism_reading, printers_publishers).
narrative_ontology:fixing_cost_class(technology_reformation_causality__technological_determinism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reproducing and distributing identical text at scale: once presses and movable type existed, the same scripture or tract could be placed in thousands of dispersed hands at a fraction of scribal cost, solving the medieval bottleneck that had kept text reproduction slow, expensive, and channel-controllable.
% TRANSFER_FUNCTION: Moves cheap reproduced text from presses to dispersed readers; moves interpretive authority over scripture from a clerical monopoly toward literate lay readers; moves the profits of the new mass text market to printer-publishers; moves displacement costs onto the scribal economy and governance costs onto every authority that had controlled text flow.
% ABSENT_VOICES: The illiterate rural laity, whose religious authority was rearranged without their participation and who remained dependent on whichever confession held their territory's pulpit; the scribal communities, whose displacement was treated as inevitable friction rather than a cost anyone owed; and the censorial authorities, whose objection — that reproduction speed had permanently outrun any licensing regime — was registered nowhere in the arrangement's own terms, only in the resistance record.
% DISAPPEARANCE_RATIONALE: Remove print's cost structure in 1517 and the Reformation's signature features do not occur on schedule: no mass vernacular Bibles, no pamphlet war, no doctrinal diffusion crossing borders faster than any counter-edict. The Church's information monopoly persists for generations and reform currents remain regional and suppressible, as the Hussite movement had been a century earlier — the determinist reading's central counterfactual evidence.
% FOUNDING_PROBLEM: The manuscript bottleneck: reproducing any text by hand cost months of skilled labor, so scripture and doctrine reached dispersed readers only through a controlled scribal and clerical chain. The press was built to break that cost bottleneck for ordinary text reproduction — liturgical books, indulgences, classical texts — not to reform religion.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by book-price series and the commercial collapse of the scriptoria, documented by historians of the book from Febvre and Martin onward: the cost problem was real and then solved. The scribal communities' own obsolescence attests the founding problem's death — no party with an interest in the determinist narrative speaks for them. The printers' guilds, by contrast, invoked the founding problem to justify their privileges, so their testimony is treated as interested; the mismatch between the dead founding problem and the world-rearranging persistence is left standing for the engine's mismatch check rather than reconciled here.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because the reading's referent assessment is grounded in production-cost reduction: the arrangement's dominant flow was subsidy — cheap identical text — and what extraction exists (imperial and civic printing privileges, later licensing compliance costs) sits just above the information_standard coordination floor of 0.02. Suppression is low (0.12): the arrangement's persistence required no coercion; it was self-sustaining economics. The century's famous coercion — the Edict of Worms, the Index, the burnings — was resistance TO the constraint, not enforcement BY it, and belongs in the resistance metric, not suppression. Theater is low (0.10): presses really ran and texts really moved; the gentle post-1517 rise (0.06 to 0.14) tracks the growing share of output that was polemical performance — the flugschriften war — rather than functional reproduction. Accessibility collapse is high (0.85): once print economics were understood, scribal reproduction collapsed as a workable alternative; no scriptorium could compete on cost. Resistance is moderate (0.45): the constraint met real, organized, well-funded resistance that failed not for lack of effort but because the cost curve outran any licensing regime. The mountain claim and these metrics are authored independently: the claim asserts naturality, while the metrics record that the 'natural law' met a century of organized resistance and left identifiable winners and losers — exactly the profile the false-summit signature exists to catch. No suppression_requirement series is authored: the constraint's own enforcement picture (privilege administration, guild market rules) is static-low across the interval, which the scalar already captures; the dynamic coercion of the period ran in the opposite direction (against the constraint). Both series run on one shared eight-point grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute sharply different types from the same low-epsilon story. From the printer seat the arrangement is an open market riding a cost curve; from the church-hierarchy seat the same arrangement is the destruction of a millennium-old interpretive monopoly by a technology that could neither be licensed nor un-invented — extraction in everything but name. The scribal seat experiences pure displacement. The determinist reading's own claim — no one extracted, physics happened — is the beneficiary-side experience generalized into a cosmology. Same-level dynamics: printers and reformers are both organized beneficiaries at comparable power, differentiated by exit (constrained capital versus identity-locked commitment); the church hierarchy and territorial rulers are both institutional actors with opposite directionalities — one trapped in the truth-claims that prevent concession, the other committed past reversal by the confiscations and confessional settlements it collected. The engine computes each seat from the structural data; the divergence between the reading's near-floor epsilon and the trapped victim seats' computed extraction is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: printers collected the mass-text market's profits; reformers received a distribution channel no authority could match — a single tract reached every press town faster than any edict could answer it; laity received scripture at prices scribal production could never reach; rulers collected the political rearrangement the channel enabled. Victim declarations: the church hierarchy bore the loss of its reproductive and interpretive monopoly (trapped — conceding vernacular scripture meant conceding the doctrinal ground), and scribal communities bore direct livelihood displacement. The derivation chain places the four beneficiary seats near the subsidy end and the trapped institutional victim near the full-target end, which is why a single story with epsilon 0.08 should still compute high effective extraction at the church and scribal seats. No directionality overrides are used: the declared structure plus exit options already produce the correct d for every seat, and the reading's own claim (no administrator, no rent-collector) is consistent with the absence of any agenda_setter seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the manuscript reproduction bottleneck — was dead by roughly 1500, solved by the arrangement itself, yet the arrangement persisted, grew, and rearranged the world around it (founding_problem_status dead + disappearance_verdict world_rearranges). The determinist reading interprets that persistence as causally productive physics: the cost curve kept working, so the arrangement kept working. The mandatrophy apparatus reads the same profile as the classic post-mandate persistence signature. The resolution is the story's pivot: if post-1500 persistence was pure cost physics, the determinist claim stands and the zombie flag is a category error; if persistence was maintained by identifiable beneficiaries — printers' markets, reformers' channel, rulers' confiscations — then the flag is accurate and the mountain was a false summit from the start. This story authors the mismatch honestly rather than reconciling it, because the reconciliation IS the kernel dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_beneficiary_construction,
    'Is the print-economics constraint a genuine structural law of information reproduction — any reproduction-cost collapse would have forced the same religious rearrangement — or a constructed arrangement whose inevitability was produced and then narrated as natural necessity by identifiable beneficiaries: printers seeking a mass market, reformers seeking an uncontrollable channel, rulers seeking confiscation and sovereignty?',
    'Comparative analysis of print adoption without Reformation: if identical cost structures in Italy, Spain, and the Habsburg lands produced no comparable rupture, the constructed account gains. Formal counterfactual modeling of the Hussite precedent (a reform attempt without print, suppressed) against the Lutheran case tests whether the channel or the actors made the difference.',
    'If constructed, the mountain claim is a false summit: the false-summit signature reclassifies toward tangled_rope, the determinist axiom loses its empirical grounding, and the sibling agency reading inherits the causal weight. If genuine, the sibling readings reduce to downstream noise and the near-floor epsilon assessment is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_beneficiary_construction, empirical, 'Whether the constraint is natural law or a beneficiary-constructed arrangement (FSM ambiguity, schema-mandated for a mountain with declared beneficiaries).').

omega_variable(
    sufficiency_vs_necessity_gap,
    'Does the historical record establish print as sufficient for the Reformation (the reading''s strong inevitability claim) or merely necessary, or strongly enabling?',
    'The Italy and Spain natural experiments (full print adoption, no Reformation) and the uneven confessional map of the Empire test sufficiency directly; diffusion-rate modeling against the 1520s pamphlet explosion tests how much of the rupture the cost curve alone predicts.',
    'If print was only necessary or enabling, the inevitability premise fails, the reading collapses toward co-constitution, and the drift vector moves from substantial toward severe. If sufficient, the counterexamples require an explanation the sibling readings have not supplied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sufficiency_vs_necessity_gap, empirical, 'Whether the evidence supports causal sufficiency or only necessity/enabling.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the technology_reformation_causality kernel — what would adopting a sibling reading change structurally, and where exactly is the disagreement located?',
    'Adjudication requires a causal framework all three reading communities accept; none exists, so each reading resolves its own epsilon over the shared referent arrangement (the 1450-1555 religious print economy). The disagreement is located in the causal-sufficiency premise: whether the operative cause is the reproduction-cost curve (this reading), the strategic deployment by printers and reformers (beneficiary_agency_reading), or joint co-evolution of technology and actors (co_constitution_reading).',
    'Adopting the beneficiary-agency reading would relocate epsilon from the technology''s cost structure to the agency structure and likely reclassify the arrangement as a rope or tangled_rope built by actors; adopting co-constitution would split causation jointly and redistribute the beneficiary/victim map. This story''s identity, claimed type, and directionality map are all conditional on the determinist premise holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story instantiates the technological_determinism_reading; sibling readings would change epsilon''s locus, the claimed type, and the directionality map.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1450, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tref_tech_det_tr_t1450, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1450, 0.06).
narrative_ontology:measurement_basis(tref_tech_det_tr_t1450, observed).
narrative_ontology:measurement(tref_tech_det_tr_t1470, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1470, 0.07).
narrative_ontology:measurement_basis(tref_tech_det_tr_t1470, observed).
narrative_ontology:measurement(tref_tech_det_tr_t1490, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1490, 0.07).
narrative_ontology:measurement_basis(tref_tech_det_tr_t1490, observed).
narrative_ontology:measurement(tref_tech_det_tr_t1510, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1510, 0.08).
narrative_ontology:measurement_basis(tref_tech_det_tr_t1510, observed).
narrative_ontology:measurement(tref_tech_det_tr_t1517, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement_basis(tref_tech_det_tr_t1517, observed).
narrative_ontology:measurement(tref_tech_det_tr_t1525, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1525, 0.11).
narrative_ontology:measurement_basis(tref_tech_det_tr_t1525, observed).
narrative_ontology:measurement(tref_tech_det_tr_t1540, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1540, 0.12).
narrative_ontology:measurement_basis(tref_tech_det_tr_t1540, observed).
narrative_ontology:measurement(tref_tech_det_tr_t1555, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1555, 0.14).
narrative_ontology:measurement_basis(tref_tech_det_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(tref_tech_det_be_t1450, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement_basis(tref_tech_det_be_t1450, observed).
narrative_ontology:measurement(tref_tech_det_be_t1470, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1470, 0.05).
narrative_ontology:measurement_basis(tref_tech_det_be_t1470, observed).
narrative_ontology:measurement(tref_tech_det_be_t1490, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1490, 0.06).
narrative_ontology:measurement_basis(tref_tech_det_be_t1490, observed).
narrative_ontology:measurement(tref_tech_det_be_t1510, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1510, 0.06).
narrative_ontology:measurement_basis(tref_tech_det_be_t1510, observed).
narrative_ontology:measurement(tref_tech_det_be_t1517, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1517, 0.07).
narrative_ontology:measurement_basis(tref_tech_det_be_t1517, observed).
narrative_ontology:measurement(tref_tech_det_be_t1525, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1525, 0.08).
narrative_ontology:measurement_basis(tref_tech_det_be_t1525, observed).
narrative_ontology:measurement(tref_tech_det_be_t1540, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1540, 0.08).
narrative_ontology:measurement_basis(tref_tech_det_be_t1540, observed).
narrative_ontology:measurement(tref_tech_det_be_t1555, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1555, 0.09).
narrative_ontology:measurement_basis(tref_tech_det_be_t1555, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__technological_determinism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, information_standard).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial causal question 'did print cause the Reformation?' decomposes, per the epsilon-invariance principle, into three structurally distinct constraint stories sharing one referent arrangement (the early-modern religious print economy) and differing in the causal premise each holds decisive. This story (technological_determinism_reading) authors epsilon at the production-cost floor and claims mountain; beneficiary_agency_reading locates epsilon in the strategic agency structure and would claim a built arrangement; co_constitution_reading splits causation jointly. Each file carries its own epsilon, beneficiaries, victims, and claimed type; they are linked here because the upstream empirical record (cost series, print-run data) is cited as evidence by all three readings, and contamination of the shared evidentiary base propagates across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
