% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Printing Press as Deterministic Cause of the Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between roughly 1440 and 1560, movable-type printing collapsed the
 *   marginal cost of reproducing text by roughly two orders of magnitude
 *   relative to manuscript copying. This reading holds that once that cost
 *   collapse occurred, mass distribution of vernacular scripture and
 *   polemical religious literature became a structural inevitability — a fact
 *   about production economics and information diffusion that no single actor
 *   authored or could have prevented, comparable to a natural-law constraint
 *   on what forms of religious dissent could scale. The claim is explicitly a
 *   mountain claim: the technology is treated as emergent,
 *   physically/economically given, not a policy choice by any party.
 *
 * KEY AGENTS:
 *   - print_shop_proprietors: incidental economic beneficiaries of the cost collapse
 *   - vernacular_literate_laity: downstream beneficiaries of the resulting access
 *   - roman_church_hierarchy: institutional actor whose prior monopoly the cost collapse structurally erodes
 *   - reformist_theologians: early adapters occupying the niche the cost collapse opened
 *   - historians_of_technology: analytical observers weighing causal strength
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.12).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.08).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press as Deterministic Cause of the Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '963da9bc-ddf0-4414-8dea-478c79defd9b').
narrative_ontology:cs_kernel_codification('963da9bc-ddf0-4414-8dea-478c79defd9b', distributed).
narrative_ontology:cs_authority_grounding('963da9bc-ddf0-4414-8dea-478c79defd9b', distributed).
narrative_ontology:cs_reading_relation('963da9bc-ddf0-4414-8dea-478c79defd9b', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('963da9bc-ddf0-4414-8dea-478c79defd9b', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('963da9bc-ddf0-4414-8dea-478c79defd9b', foundational, production_cost_collapse_is_sufficient_cause).
narrative_ontology:cs_axiom_status(production_cost_collapse_is_sufficient_cause, holdable).
narrative_ontology:cs_axiom_grounding('963da9bc-ddf0-4414-8dea-478c79defd9b', production_cost_collapse_is_sufficient_cause, empirically_contingent).
narrative_ontology:cs_axiom('963da9bc-ddf0-4414-8dea-478c79defd9b', secondary, human_strategic_choice_is_causally_epiphenomenal_to_technology).
narrative_ontology:cs_axiom_status(human_strategic_choice_is_causally_epiphenomenal_to_technology, holdable).
narrative_ontology:cs_axiom_grounding('963da9bc-ddf0-4414-8dea-478c79defd9b', human_strategic_choice_is_causally_epiphenomenal_to_technology, empirically_contingent).
narrative_ontology:cs_reference_frame('963da9bc-ddf0-4414-8dea-478c79defd9b', scribal_manuscript_monopoly_baseline).
narrative_ontology:cs_drift_state('963da9bc-ddf0-4414-8dea-478c79defd9b', post_gutenberg_diffusion_1500, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('963da9bc-ddf0-4414-8dea-478c79defd9b', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, print_shop_proprietors).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_literate_laity).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, movable_type_production_cost_collapse_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, media_ecology_determinism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate movable-type workshops that can reproduce a vernacular Bible or pamphlet at a fraction of scribal cost. On this reading they are not strategic religious actors but economic agents responding to a fixed technical fact: unit cost of a copied page has collapsed by roughly two orders of magnitude. They profit incidentally from whatever text sells, doctrinal content included.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, print_shop_proprietors, beneficiary,
    moderate, biographical, mobile, regional).

% Gain access to scripture and devotional literature in their own language at prices previously unreachable. On the determinism reading their consumption is the predictable downstream effect of a fixed distribution-cost curve, not a chosen alliance with reformers — once the press exists, this access follows as a matter of production physics and market diffusion, not deliberate design by any party.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_literate_laity, beneficiary,
    powerless, generational, constrained, regional).

% Held the prior monopoly on scriptural interpretation, sustained by the cost and scarcity of hand-copied manuscripts. On this reading the hierarchy is not defeated by any strategic actor's choice but by an irreversible drop in the marginal cost of producing a vernacular text — a structural fact it can slow at the margins (licensing, indices of forbidden books) but cannot reverse, because the underlying production technology does not un-invent itself.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, roman_church_hierarchy, excluded,
    institutional, civilizational, trapped, continental).

% Write and circulate vernacular theological arguments. On the determinism reading their agency is real but causally secondary: they are early adapters occupying a niche the press's cost structure opened, not authors of the outcome. This story deliberately reads them as downstream of the technology rather than as strategic deployers of it (that framing belongs to the sibling beneficiary_agency_reading, not to this constraint).
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformist_theologians, excluded,
    moderate, generational, constrained, continental).

% Assess the causal weight of the printing press against social, political, and religious factors in the Reformation's spread, comparing print-diffusion maps against confessional boundaries to test how much variance the technology alone explains.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination-of-interests sense — this reading describes a physical/economic fact (a step-change reduction in per-unit reproduction cost for text) propagating through a population, not an agreement among parties to solve a shared problem.
% TRANSFER_FUNCTION: The press does not transfer value between named parties on this reading; it lowers a universal production cost, which then reallocates who can afford to reach an audience — from scribal guilds and manuscript patrons toward printers and vernacular readers, as an emergent consequence of the cost curve rather than a deliberate transfer.
% ABSENT_VOICES: The sibling readings' central claims — that reformers strategically weaponized print (beneficiary_agency_reading) or that press and reformers co-shaped each other (co_constitution_reading) — are structurally excluded from this reading by design; this story treats their agency as adaptation to a fixed constraint, which those readings would dispute as understating human strategic choice.
% DISAPPEARANCE_RATIONALE: On strict determinism, if the printing press had not existed the vernacular scripture distribution mechanism would not have existed either and Reformation-scale doctrinal fragmentation would have been structurally blocked or much slower — the world rearranges. But historians dispute this counterfactual (manuscript networks, prior heresies like Lollardy and Hussitism achieved regional vernacular spread without print), so the verdict is contested even within a determinism-sympathetic frame.
% FOUNDING_PROBLEM: Movable type was developed to solve a production-economics problem — the prohibitive labor cost and error rate of hand-copying texts — not a religious problem; its application to scripture is this reading's central causal claim.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the printing trade (outside both the Church hierarchy and the reformist camp) attest the original commercial motive was Gutenberg's search for a scalable book-production business, corroborated by early print-shop ledgers and the initial dominance of liturgical and classical texts over polemical ones in the first print decades.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.12, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.12 at interval end) because on this reading the press itself extracts nothing from anyone — it is a production-cost fact, not an enforced arrangement; any low residual extraction reflects printers' incidental commercial margins, not coercive rent. Suppression is authored low (0.08) because the technology does not itself suppress alternatives — the Church's later licensing and censorship regimes are separate, downstream, human-authored constraints not modeled in this story. Accessibility collapse is authored high (0.8) because once movable type exists, hand-copying as the dominant reproduction technology becomes economically nonviable almost everywhere it spreads — that collapse of the prior method is the mountain's signature. Resistance is authored low (0.2): a production-cost fact meets little direct resistance qua technology, even though the doctrinal content it carries meets substantial resistance (that resistance belongs to a different constraint, not this one).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (printers, vernacular laity) are declared with low derived d because the cost collapse subsidizes their access/output without extracting from them structurally. No victims are declared on this reading — the determinism frame does not identify a party the printing press extracts from; the Church hierarchy's loss of monopoly is a competitive-structure effect, not an extractive one, which is why roman_church_hierarchy is listed as excluded/observer-adjacent rather than as a victim requiring base_properties.victims. This is a deliberate structural choice distinguishing this reading from a snare-flavored counter-reading that might cast Church revenue loss as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in a specific direction: because it treats the press as a mountain (naturally emergent, no active enforcement), the classification correctly avoids painting a physical technology as tangled_rope or snare merely because contested religious content rode on it. The founding_problem is authored as dead (Gutenberg's original commercial press problem) precisely to keep separate the technology's original function from its later religious-political effects, so the story does not smuggle in extraction it cannot structurally justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_agency_causal_weight,
    'Is the Reformation''s spread better explained by the printing press''s cost-structure change alone, or by strategic choices of reformers and printers who selected which texts to print and where to distribute them?',
    'Comparative diffusion-mapping studies correlating print-shop density and output composition against confessional adoption timelines, controlling for prior literacy rates and existing heretical networks (Hussite, Lollard) that spread vernacular dissent pre-press.',
    'If cost-structure alone predicts adoption patterns independent of strategic printer/reformer choices, the determinism reading gains support and the sibling beneficiary_agency_reading''s causal claim weakens. If strategic selection effects dominate (certain texts printed disproportionately, certain regions targeted), the agency reading''s causal claim strengthens and this story''s mountain classification becomes contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_vs_agency_causal_weight, empirical, 'Whether print-cost economics or strategic human agency carries the primary causal weight for Reformation spread.').

omega_variable(
    mountain_vs_constructed_beneficiary_structure,
    'Does declaring print_shop_proprietors and vernacular_literate_laity as beneficiaries on a claimed mountain constraint indicate the technology was in fact a constructed/exploitable arrangement rather than a pure natural-law-like constraint?',
    'Trace whether early printers deliberately targeted profitable religious controversy (evidence of strategic exploitation, which would favor reclassification toward tangled_rope via FSM) versus printing religious texts merely as one commercially viable content category among many (consistent with genuine mountain framing).',
    'If printers systematically prioritized inflammatory religious content because it was more profitable than neutral texts, this indicates the ''natural'' cost-collapse story is partly cover for opportunistic commercial exploitation of religious conflict — false summit mountain (FSM) territory. If content selection was largely uniform/non-strategic, the mountain framing holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mountain_vs_constructed_beneficiary_structure, conceptual, 'Whether beneficiary presence on this mountain claim signals a false summit or a genuine incidental-beneficiary structure.').

omega_variable(
    cs_framing_kernel_vs_authority_layer,
    'Should this constraint be framed around the printing press as kernel-technology, or around the Church''s interpretive monopoly as the contested authority layer the technology destabilizes?',
    'Compare classification outcomes under a framing centered on the press (kernel = production technology, this story''s choice) versus a framing centered on scriptural interpretive authority (kernel = who may authoritatively interpret scripture, with the press as an exogenous shock to that authority structure).',
    'The press-centered framing (chosen here) yields a mountain/low-extraction classification. An authority-centered framing would likely classify the pre-Reformation interpretive monopoly itself as a tangled_rope or snare being destabilized, which is a materially different constraint with a different claimed_type and different beneficiary/victim structure. This story deliberately adopts the press-centered framing per the assigned technological_determinism_reading; the authority-centered framing belongs to a different constraint entirely, not a parameter of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_authority_layer, conceptual, 'Alternative framing (technology-as-kernel vs. authority-as-kernel) would change the classification; this story deliberately adopts the former per its assigned reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1440, 1560).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1440, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1440, 0.05).
narrative_ontology:measurement(tech_tr_t1460, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1460, 0.07).
narrative_ontology:measurement(tech_tr_t1480, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1480, 0.09).
narrative_ontology:measurement(tech_tr_t1500, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(tech_tr_t1520, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1520, 0.13).
narrative_ontology:measurement(tech_tr_t1540, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1540, 0.14).
narrative_ontology:measurement(tech_tr_t1560, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1560, 0.15).

% Extraction over time
narrative_ontology:measurement(tech_be_t1440, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1440, 0.05).
narrative_ontology:measurement(tech_be_t1460, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1460, 0.06).
narrative_ontology:measurement(tech_be_t1480, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1480, 0.08).
narrative_ontology:measurement(tech_be_t1500, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1500, 0.09).
narrative_ontology:measurement(tech_be_t1520, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1520, 0.11).
narrative_ontology:measurement(tech_be_t1540, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1540, 0.12).
narrative_ontology:measurement(tech_be_t1560, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1560, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__technological_determinism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__technological_determinism_reading, 0.02).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the technology_reformation_causality kernel. beneficiary_agency_reading treats reformers/printers as strategic agents deploying a tool (likely rope or tangled_rope, with reformist_theologians and print_shop_proprietors as active agenda-setters, not downstream adapters). co_constitution_reading treats press and social actors as mutually shaping outcomes over time (likely a different ε trajectory reflecting escalating co-adaptation rather than a flat cost-collapse curve). This story (technological_determinism_reading) claims mountain status with low, flat extraction, since it locates causal weight entirely in the exogenous technology rather than in any party's strategic choice. The three stories are linked here rather than merged because their claimed_type, ε values, and beneficiary/victim structures are genuinely distinct — per the ε-invariance principle, this is three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
