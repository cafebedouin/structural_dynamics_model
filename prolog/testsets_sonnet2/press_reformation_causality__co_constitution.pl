% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Economy/Religious-Controversy Feedback Loop as Co-Constituting Infrastructure
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the co_constitution reading of the
 *   press_reformation_causality kernel: the claim that print technology and
 *   the religious-controversy actors who used it formed a bidirectional
 *   feedback loop, each reshaping the other's capacities and incentives, such
 *   that neither the technology's affordances nor the reformers' agency alone
 *   explains the Reformation's scale and trajectory. Print capacity created a
 *   market for controversial vernacular content; controversy created
 *   sustained commercial demand that funded expansion of print infrastructure
 *   across confessional lines. Under this reading, the technology functions
 *   as a scaffold — enabling infrastructure whose justification is the
 *   transition it made possible, not a permanent extractive arrangement —
 *   while multiple tangled_rope-flavored relationships exist between specific
 *   actor pairs (printers/reformers, printers/Church, princes/reformers)
 *   nested inside the larger scaffold, none of which the co_constitution
 *   frame reduces to a single beneficiary or a single determining cause.
 *
 * KEY AGENTS:
 *   - printers_and_publishers: primary infrastructure operators — mobile, organized, capture commercial rents from controversy-driven demand
 *   - reforming_clergy: primary content generators — moderate power, constrained exit, capture doctrinal authority through the print channel they do not own
 *   - catholic_hierarchy_revenue_streams: primary bearer of authority/revenue transfer — institutional power but structurally trapped, cannot match print's decentralized responsiveness
 *   - literate_urban_laity: secondary beneficiary — gains direct interpretive access previously mediated by clergy
 *   - illiterate_rural_populations: excluded from the discourse but not from its material consequences
 *   - dissenting_printers_outside_orthodoxy: bear suppression once the mainstream reform/prince coalition consolidates around a narrower confessional settlement
 *   - territorial_princes_and_magistrates: analytical/steering seat — did not create the loop but learn to exploit it for political consolidation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.42).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.38).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, scaffold).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Economy/Religious-Controversy Feedback Loop as Co-Constituting Infrastructure").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

narrative_ontology:has_sunset_clause(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, 'c8da2f43-183f-40f0-9424-0d2236687d7e').
narrative_ontology:cs_kernel_codification('c8da2f43-183f-40f0-9424-0d2236687d7e', distributed).
narrative_ontology:cs_authority_grounding('c8da2f43-183f-40f0-9424-0d2236687d7e', distributed).
narrative_ontology:cs_reading_relation('c8da2f43-183f-40f0-9424-0d2236687d7e', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('c8da2f43-183f-40f0-9424-0d2236687d7e', press_reformation_causality__strategic_deployment, influences).
narrative_ontology:cs_axiom('c8da2f43-183f-40f0-9424-0d2236687d7e', foundational, causal_irreducibility_of_agency_and_technology).
narrative_ontology:cs_axiom_status(causal_irreducibility_of_agency_and_technology, holdable).
narrative_ontology:cs_axiom_grounding('c8da2f43-183f-40f0-9424-0d2236687d7e', causal_irreducibility_of_agency_and_technology, empirically_contingent).
narrative_ontology:cs_axiom('c8da2f43-183f-40f0-9424-0d2236687d7e', secondary, feedback_loop_precludes_single_locus_beneficiary).
narrative_ontology:cs_axiom_status(feedback_loop_precludes_single_locus_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('c8da2f43-183f-40f0-9424-0d2236687d7e', feedback_loop_precludes_single_locus_beneficiary, empirically_contingent).
narrative_ontology:cs_created_at('c8da2f43-183f-40f0-9424-0d2236687d7e', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printers_and_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reforming_clergy).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, literate_urban_laity).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_hierarchy_revenue_streams).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, illiterate_rural_populations).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, dissenting_printers_outside_orthodoxy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, territorial_princes_and_magistrates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate presses across German, Swiss, and Low Countries cities. They select which tracts to typeset and distribute based on anticipated sales, not doctrinal loyalty alone. Reformation pamphlets sell fast and cheap; printers who serve reforming markets prosper, while those tied to Church patronage in contested regions lose ground. They can relocate operations across jurisdictional lines when local authorities crack down, giving them real mobility the theological actors lack.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printers_and_publishers, agenda_setter,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printers_and_publishers, beneficiary).

% Figures like Luther and his allies write tracts whose reach depends entirely on print capacity and distribution networks they do not own. Their theological arguments shape what printers choose to run, but the printers' commercial calculus and technical capacity shape what forms those arguments take (short pamphlets, vernacular, illustrated). Exit from the loop would mean abandoning the only mass-reach channel available; most are ideologically and institutionally committed to staying inside it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reforming_clergy, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reforming_clergy, agenda_setter).

% Indulgence sales, tithe compliance, and doctrinal uniformity that generated revenue and authority are directly undercut by pamphlet circulation exposing abuses and offering alternative theology. The Church attempts counter-printing and censorship but cannot match the market responsiveness of the decentralized print economy; its territorial and hierarchical structure cannot relocate the way a print shop can.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_hierarchy_revenue_streams, payer,
    institutional, generational, trapped, continental).

% Urban readers gain unprecedented direct access to vernacular scripture and theological argument, bypassing clerical mediation. They can choose which pamphlets to buy, which preachers to follow, and in many cases which city to live in as confessional lines harden — a new form of agency the pre-print information environment did not offer them.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, literate_urban_laity, beneficiary,
    moderate, biographical, mobile, regional).

% Cannot read the pamphlets driving the controversy and depend on oral transmission, sermons, and secondhand report for any understanding of the theological stakes. Their communities are nonetheless swept into confessional conflict, taxation disputes, and occasionally violence generated by a controversy conducted largely in a medium closed to them.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, illiterate_rural_populations, excluded,
    powerless, biographical, trapped, regional).

% Printers who ran radical or heterodox material (Anabaptist tracts, peasant manifestos) beyond what either reforming princes or Catholic authorities would tolerate faced seizure of presses, exile, or execution. The same infrastructure that empowered mainstream reform closed ranks against material too far outside the emerging confessional settlements.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, dissenting_printers_outside_orthodoxy, payer,
    powerless, biographical, trapped, local).

% Watch the print/controversy loop reshape the legitimacy calculus in their territories and intervene selectively — licensing sympathetic presses, banning others — to consolidate political authority. They did not create the feedback loop but learned to steer parts of it once it existed.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, territorial_princes_and_magistrates, observer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, territorial_princes_and_magistrates, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The print economy solved a genuine distribution problem for theological argument (rapid, cheap, vernacular reproduction) while religious controversy solved a genuine demand problem for print capacity (guaranteed audience for pamphlets, driving investment in presses and distribution networks). Neither could have scaled as it did without the other; the coordination is bidirectional infrastructure-building, not one party using a tool on another.
% TRANSFER_FUNCTION: Moves religious authority and revenue away from centralized clerical mediation (indulgences, tithe compliance, monopoly on scriptural interpretation) toward decentralized nodes: print-shop owners who capture commercial rents, reforming clergy who capture doctrinal authority and lay allegiance, and literate laity who capture direct interpretive access. Illiterate populations and heterodox printers bear costs of the resulting instability without commensurate access to the new authority structures.
% ABSENT_VOICES: Illiterate rural populations are swept into confessional conflict and its material costs (war, taxation, displacement) without participating in the print-mediated discourse that generated it; their objections, where recorded at all, survive only through hostile clerical or magisterial sources. Heterodox printers who pushed the technology toward radical ends were suppressed by the very coalition (reformers + sympathetic princes) that had benefited from the same infrastructure.
% DISAPPEARANCE_RATIONALE: Remove either half of the loop — printing capacity or the controversy generating demand for it — and the trajectory changes materially. Manuscript-era heresies (Hus, Wycliffe) were geographically contained and eventually suppressed; absent print's replication capacity, Luther's movement plausibly follows a similar contained trajectory. Absent the controversy, print technology develops along its actual historical path toward commercial and administrative uses without the mass ideological mobilization that made confessional Europe. The co-constitution reading holds that neither factor alone explains the outcome; removing either changes the world substantially.
% FOUNDING_PROBLEM: Neither party set out to build this loop as a unified project. Printers sought profitable content; reformers sought a scriptural-authority argument against an entrenched hierarchy perceived (by reformers) as illegitimately arrogating interpretive and financial authority. The 'founding problem' this reading names is retrospective: how did two independently motivated activities generate a self-reinforcing system neither side fully controlled?
% FOUNDING_PROBLEM_CORROBORATION: Media historians (Eisenstein, and critically revised by Pettegree and Green) attest the feedback dynamic from outside both the printing trade's own guild memory and the confessional traditions' own founding narratives — both of which tend toward simpler determinist or heroic-agency stories. Economic historians of the early print trade corroborate the commercial-demand side independently of theological sources. No living institutional party benefits from asserting the co-constitution reading specifically, which is itself why it survives mainly in academic historiography rather than in either the printing trade's or the reform tradition's self-narration.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).
:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from a low baseline (0.15 at 1450, before Reformation controversy exists) to a mid-range peak (0.45 around 1555, during peak confessional conflict and princely consolidation) before declining somewhat as settlements (Peace of Augsburg-era arrangements) stabilize authority transfers into new institutional forms. This trajectory is consistent with a scaffold: extraction is real but transitional, tied to the disruption/settlement cycle rather than to a permanent extractive steady state. Theater ratio stays comparatively low throughout (0.1-0.25) because both the print economy and the theological controversy were substantively functional activities — pamphlets were read, arguments were engaged, presses produced real distributional capacity — rather than performative maintenance of an atrophied function. Suppression (0.38) and accessibility_collapse (0.35) are moderate: alternatives to the print-mediated discourse (oral transmission, manuscript circulation, clerical mediation) did not vanish, they were out-competed on cost and reach, which is a coordination dynamic more than a coercive one, consistent with the co_constitution reading's rejection of a single suppressing party.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers and reforming clergy sit toward the beneficiary end: printers through commercial capture with genuine mobility (relocatable presses), clergy through doctrinal-authority capture despite constrained personal exit. The Catholic hierarchy's revenue streams sit at the target end — institutionally powerful in the abstract but structurally trapped relative to this specific loop, unable to relocate the way a press can. Illiterate rural populations and dissenting printers are the clearest targets: powerless, trapped or locally rooted, bearing costs (confessional violence, suppression) generated by a discourse loop they cannot access or that turns on them once radical. Literate urban laity sit closer to symmetric-beneficiary: real gains in interpretive access, real exposure to the instability the loop generates.
 *
 * MANDATROPHY ANALYSIS:
 *   The sunset clause here is structural rather than declared by any single party: the scaffold's justification is the transition from clerically-mediated to print-mediated religious authority, and once confessional settlements stabilize (roughly by 1600, per the declining extractiveness trend), the acute coordination/extraction dynamics of the loop itself subside into settled institutional forms (state churches, licensing regimes) that are better modeled as separate, later constraints rather than as this scaffold persisting past its function. Treating the print/controversy loop as a permanent extractive arrangement (rather than transitional infrastructure) would mislabel a genuine, historically bounded co-constitution process as pure extraction — the corrective the scaffold classification is meant to provide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_constitution_vs_determinism_locus,
    'Is the mutual-shaping dynamic this story claims genuinely irreducible to either a technology-first or agency-first account, or is co-constitution language obscuring an underlying asymmetry where one factor was in fact doing most of the causal work?',
    'Comparative case analysis against contained heresy movements pre-print (Hus, Wycliffe, Lollards) and against post-print controversies that did not scale (various suppressed continental movements) to isolate whether print capacity alone or reformer strategy alone tracks better with which movements scaled and which did not.',
    'If comparative cases show print capacity is necessary but agency-driven strategic choices are not (i.e., any sufficiently motivated movement scales given print access), the technological_determinism sibling reading gains support and this reading''s classification of technology as a co-equal, non-deterministic scaffold factor would need revision. If cases show strategic choice matters more than access, the strategic_deployment reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_vs_determinism_locus, conceptual, 'Whether co-constitution is a genuine third account or a hedge between the two sibling readings.').

omega_variable(
    distributed_extraction_measurement,
    'Does ''no single beneficiary, distributed extraction'' accurately describe the structure, or does it understate concentration among the printer class specifically, who profited commercially regardless of which side of the controversy ultimately prevailed in a given territory?',
    'Economic-historical reconstruction of print-shop profitability across confessional outcomes (regions that stayed Catholic vs. regions that turned Protestant) to test whether printers captured comparable rents independent of doctrinal outcome, which would suggest printers were a more concentrated beneficiary than the distributed framing allows.',
    'If printer profitability was outcome-independent, this reading''s ''no single beneficiary'' claim weakens and a printer-centered tangled_rope or even snare-flavored sub-story would be warranted as a more precise decomposition, alongside this scaffold story rather than replacing it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_extraction_measurement, empirical, 'Whether printers constitute a concentrated beneficiary class understated by the distributed-extraction framing.').

omega_variable(
    scaffold_sunset_dating,
    'When does the co-constitution scaffold''s transitional function actually end — at the Peace of Augsburg (1555), at the Peace of Westphalia (1648, outside this story''s interval), or is there no clean sunset because print-mediated religious controversy persists in altered form indefinitely?',
    'Track whether post-1600 religious print controversy exhibits the same feedback structure (commercial press capacity shaping and shaped by controversy demand) or a structurally different pattern (state-licensed confessional printing, reduced novelty-driven demand).',
    'A clean sunset supports the scaffold classification as authored; an indefinite persistence would suggest either extending the interval or reclassifying the mature phase as a distinct, later constraint (a state-church print licensing regime) rather than this transitional co-constitution story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_dating, empirical, 'Whether the scaffold''s transitional period has a clean historical endpoint within the authored interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__co_constitution, theater_ratio, 1450, 0.1).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causality__co_constitution, theater_ratio, 1480, 0.12).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__co_constitution, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causality__co_constitution, theater_ratio, 1530, 0.2).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__co_constitution, theater_ratio, 1555, 0.25).
narrative_ontology:measurement(pres_tr_t1580, press_reformation_causality__co_constitution, theater_ratio, 1580, 0.22).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__co_constitution, theater_ratio, 1600, 0.2).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__co_constitution, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causality__co_constitution, base_extractiveness, 1480, 0.2).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__co_constitution, base_extractiveness, 1517, 0.3).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causality__co_constitution, base_extractiveness, 1530, 0.42).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__co_constitution, base_extractiveness, 1555, 0.45).
narrative_ontology:measurement(pres_be_t1580, press_reformation_causality__co_constitution, base_extractiveness, 1580, 0.4).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__co_constitution, base_extractiveness, 1600, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causality__co_constitution, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__co_constitution, 0.05).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causality kernel. technological_determinism treats the press as an autonomous, near-mountain enabling cause; strategic_deployment treats reformers and printers as calculating strategic agents deploying the technology for religious/economic ends (closer to a tangled_rope led by identifiable strategic actors); this co_constitution reading treats technology and agency as mutually shaping through feedback, yielding a scaffold with nested, non-unified tangled_rope sub-dynamics and no single concentrated beneficiary. All three share the same underlying historical episode but author different ε, different beneficiary/victim structures, and different classifications, per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
