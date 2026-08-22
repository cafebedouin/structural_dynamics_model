% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Economy/Reformation Feedback Scaffold (Co-Constitution Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between roughly 1450 and 1650, the printing press and the theological
 *   controversies of the Reformation formed a genuine feedback structure: the
 *   press did not simply transmit pre-formed reformist ideas, and reformers
 *   did not simply master a neutral technology. Print runs responded to what
 *   sold; what sold shaped how reformers wrote; how reformers wrote shaped
 *   what readerships formed and what those readerships demanded next. The
 *   scaffold is the enabling print-economy infrastructure (presses,
 *   distribution networks, vernacular literacy growth) that made this loop
 *   possible and was itself transformed by the controversy it enabled — it
 *   functioned as transitional infrastructure whose original coordination
 *   purpose (faster circulation of argument) was eventually absorbed into
 *   settled national/confessional print industries and state censorship
 *   regimes, at which point the scaffold's original open-ended function
 *   sunsetted into routine, licensed publishing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.42).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.51).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, scaffold).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Economy/Reformation Feedback Scaffold (Co-Constitution Reading)").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).
narrative_ontology:has_sunset_clause(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, 'fe75c92d-05d7-4cc3-b254-8887100e3956').
narrative_ontology:cs_kernel_codification('fe75c92d-05d7-4cc3-b254-8887100e3956', distributed).
narrative_ontology:cs_authority_grounding('fe75c92d-05d7-4cc3-b254-8887100e3956', distributed).
narrative_ontology:cs_reading_relation('fe75c92d-05d7-4cc3-b254-8887100e3956', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('fe75c92d-05d7-4cc3-b254-8887100e3956', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('fe75c92d-05d7-4cc3-b254-8887100e3956', foundational, agency_and_artifact_are_mutually_constitutive).
narrative_ontology:cs_axiom_status(agency_and_artifact_are_mutually_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('fe75c92d-05d7-4cc3-b254-8887100e3956', agency_and_artifact_are_mutually_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('fe75c92d-05d7-4cc3-b254-8887100e3956', secondary, no_single_causal_locus_explains_reformation_spread).
narrative_ontology:cs_axiom_status(no_single_causal_locus_explains_reformation_spread, holdable).
narrative_ontology:cs_axiom_grounding('fe75c92d-05d7-4cc3-b254-8887100e3956', no_single_causal_locus_explains_reformation_spread, empirically_contingent).
narrative_ontology:cs_reference_frame('fe75c92d-05d7-4cc3-b254-8887100e3956', manuscript_era_circulation_bottleneck).
narrative_ontology:cs_drift_state('fe75c92d-05d7-4cc3-b254-8887100e3956', post_confessional_settlement_1600s, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fe75c92d-05d7-4cc3-b254-8887100e3956', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, commercial_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reforming_clergy).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, literate_urban_laity).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, unlicensed_pamphleteers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, peasant_readers_excluded_by_literacy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, dissenting_minorities_within_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, territorial_princes).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, roman_church_hierarchy).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, co_constitution_of_technology_and_agency).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, mutual_shaping_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Print shops in Wittenberg, Strasbourg, Basel, and Antwerp decide what to typeset, at what print run, and in what vernacular. They chase whatever sells — Luther's pamphlets outsold indulgence certificates by orders of magnitude — and their commercial choices shape which theological voices scale. They can relocate between cities offering different degrees of censorship tolerance, but capital tied up in type and presses limits how fast they can move.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, commercial_printers, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, commercial_printers, beneficiary).

% Luther, Zwingli, and their networks discover that print amplifies their arguments faster than they can fully control the content or interpretation reaching readers. They adapt their rhetorical style to the pamphlet form (short, vernacular, polemical) in response to what sells and what provokes response, and are themselves reshaped by the medium they deploy — a feedback loop, not a one-way tool use.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reforming_clergy, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reforming_clergy, agenda_setter).

% Merchants, guild members, and literate townspeople gain direct access to vernacular scripture and controversy literature for the first time, forming reading circles and debate networks that neither printers nor reformers fully anticipated or controlled. Their appetite for particular arguments feeds back into what gets printed next.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, literate_urban_laity, beneficiary,
    moderate, biographical, mobile, regional).

% Bears the loss of doctrinal monopoly and revenue (indulgence sales, tithes) as print outpaces the Church's own counter-publication and censorship capacity. Attempts index-of-forbidden-books enforcement and counter-pamphleteering, but the printing infrastructure it also uses for its own bulls and missals cannot be selectively disabled without disabling its own communications.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, roman_church_hierarchy, payer,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, roman_church_hierarchy, agenda_setter).

% Independent printers and writers without guild protection or noble patronage who are prosecuted, imprisoned, or executed for unlicensed religious printing when either side's authorities tighten enforcement. They bear the sharpest edge of the suppression apparatus that both reformers and Church erect around the same technology.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, unlicensed_pamphleteers, payer,
    powerless, immediate, trapped, local).

% The overwhelming majority of the rural population cannot read the vernacular pamphlets at all; the print-driven controversy proceeds largely over their heads through intermediaries (priests, itinerant preachers) who selectively relay content, meaning the print economy's benefits accrue disproportionately to literate townspeople while illiterate rural populations absorb the social and sometimes violent consequences of the resulting conflicts.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, peasant_readers_excluded_by_literacy, payer,
    powerless, generational, trapped, regional).

% Anabaptists, spiritualists, and other radical reformers who use the same print infrastructure but get suppressed by the mainstream reform movements once those movements achieve political settlements with territorial princes — the coordination technology that empowered the initial break from Rome becomes a tool for policing the boundaries of acceptable reform.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, dissenting_minorities_within_reform_movements, payer,
    moderate, biographical, constrained, regional).

% German princes and city councils use the print-fueled controversy to negotiate expanded sovereignty from both Emperor and Pope, licensing or suppressing printers according to their own territorial religious settlements (cuius regio, eius religio). They did not create the print economy but position themselves to capture political rents from its religious effects.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, territorial_princes, beneficiary,
    institutional, generational, arbitrage, regional).

% Study the archival record of print runs, correspondence, and enforcement actions to trace how technological capacity and religious agency shaped each other iteratively, without collapsing the account into either pure technological determinism or pure strategic instrumentalism.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The print economy solves a genuine coordination problem: rapid, low-cost replication and distribution of vernacular argument across geographically dispersed reading publics, allowing theological and political positions to be tested, refined, and propagated faster than manuscript culture or oral preaching alone could support.
% TRANSFER_FUNCTION: Moves theological authority and its associated revenue (indulgences, tithes, printing profits, political sovereignty) from the Roman Church hierarchy toward printers, reforming clergy, literate urban populations, and territorial princes — while moving risk and violence onto unlicensed pamphleteers, illiterate rural populations, and radical dissenters who fall outside the settlements that eventually stabilize.
% ABSENT_VOICES: Illiterate peasants, whose theological fate is decided in a controversy conducted almost entirely in a medium they cannot access directly, and radical/dissenting reformers who are written out of the historical settlement once magisterial reformers and princes reach political accommodation — both groups appear in the archival record mostly as objects of others' decisions, not as authors of pamphlets.
% DISAPPEARANCE_RATIONALE: If the print-economy/controversy feedback loop had not existed — if either the technology or the appetite for vernacular religious argument were absent — the Reformation's trajectory changes materially: without scalable replication, Luther's arguments plausibly follow the pattern of earlier reform movements (Hus, Wycliffe) that were regionally contained and eventually suppressed; without an audience whose reading practices and demands shaped print output, the technology alone produces a different, less theologically combustible outcome. The co-constitution reading holds that removing either side of the loop changes the outcome, not merely its speed.
% FOUNDING_PROBLEM: No single arrangement was 'built to solve' a problem here — the constraint names an emergent feedback structure, not a designed institution. The nearest analog to a founding problem is the pre-existing manuscript-era bottleneck on theological argument circulation, which limited how fast and how widely any challenge to Church doctrine could travel or accumulate a following.
% FOUNDING_PROBLEM_CORROBORATION: Book-historians and economic historians outside any confessional tradition (e.g., quantitative studies of print-run records and price data from printing-house archives) corroborate that the replication bottleneck the co-constitution loop responded to no longer exists in any form relevant to religious controversy; no living institution has an interest in claiming the bottleneck persists, so this status is not contested by any benefiting party.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises through the early Reformation (0.18 to ~0.44 by mid-16th century) as printers, magisterial reformers, and territorial princes progressively capture rents from the controversy — commercial print profits, doctrinal authority, political sovereignty — while suppression against unlicensed and radical printers intensifies in step (peaking near 0.55 as princely and confessional settlements harden). Both then decline modestly after 1600 as the print economy institutionalizes into licensed, lower-intensity confessional publishing — consistent with a scaffold whose acute coordination phase has passed. Theater ratio stays comparatively low throughout (peaking at 0.24) because most of the apparatus (censorship, licensing, counter-pamphleteering) performed a real function of contest management rather than pure performance, though the post-1600 plateau shows increasing ritualization of licensing procedures.
 *
 * PERSPECTIVAL GAP:
 *   From the printer's seat this looks like commercial opportunity meeting demand (near-rope); from the Church hierarchy's seat it looks like an uncontrollable extraction of authority it cannot suppress without disabling its own communications (tangled_rope/snare-adjacent); from the radical dissenter's seat, the same infrastructure that enabled the initial break becomes the instrument of their later suppression by the reform movements they helped launch. The engine should compute markedly different per-seat classifications from this one structural dataset — that divergence is the point of the co-constitution reading, which holds that no single vantage owns the causal story.
 *
 * DIRECTIONALITY LOGIC:
 *   No single beneficiary captures the extraction; it is genuinely distributed — printers profit commercially, reforming clergy gain doctrinal reach and authority, literate urban laity gain access and voice, and territorial princes gain negotiated sovereignty, while costs land on unlicensed pamphleteers (direct violence), illiterate rural populations (excluded from the medium yet exposed to its downstream conflicts), and radical dissenters (suppressed once mainstream reform stabilizes). This distributed structure is exactly what the co-constitution reading predicts and the tangled_rope dynamics between printers/reformers/Church each carry their own local beneficiary-victim pairing rather than one master asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (manuscript-era circulation bottleneck) is genuinely dead — modern print and mass communication solved it centuries ago — yet aspects of the resulting institutional apparatus (confessional censorship regimes, print licensing) persisted well past the point where the original coordination problem existed, which is consistent with scaffold-to-piton drift in the post-1600 settlement era rather than continued live coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    locus_of_agency_ambiguity,
    'Is the causal primacy in the Reformation''s spread better located in the press''s inherent capacities (determinism), in reformers'' and printers'' deliberate strategic choices (strategic_deployment), or in the iterative loop between the two (co_constitution, this reading)?',
    'Comparative case analysis: contrast regions/periods where print capacity existed without matching reformist demand (early print runs of unremarkable content) against regions with reformist demand but suppressed print access (post-1530s Catholic territories) to isolate whether outcomes track technology, intent, or their interaction.',
    'If outcomes track technological capacity alone regardless of human strategic variation, this reading''s core premise weakens toward technological_determinism; if outcomes track strategic intent regardless of the technology''s material constraints, it weakens toward strategic_deployment. Persistent interaction effects across cases would corroborate co_constitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(locus_of_agency_ambiguity, conceptual, 'Where causal primacy sits among technology, agency, and their interaction — the kernel''s central contest.').

omega_variable(
    distributed_extraction_measurability,
    'Can a genuinely distributed extraction pattern (no single dominant beneficiary) be reliably distinguished from a extraction pattern with a well-hidden single beneficiary that historical record simply fails to surface?',
    'Archival economic history tracing wealth and authority accumulation across printers, clergy, and princes over the full interval to check whether apparent distribution collapses toward concentration when longer time horizons are examined.',
    'If concentration emerges over a longer horizon (e.g., territorial princes ultimately capturing disproportionate sovereignty gains), the tangled_rope characterization understates asymmetry and a snare-leaning reading becomes more defensible for the princely seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_extraction_measurability, empirical, 'Whether the distributed-beneficiary structure holds up under longer-horizon economic-historical scrutiny.').

omega_variable(
    scaffold_sunset_dating,
    'At what point did the print-economy scaffold''s transitional coordination function actually sunset into routinized, non-transitional print industry — is 1600 too early, or does elements of the original open-ended coordination function persist into the 18th century?',
    'Trace licensing regime formalization dates and print-run diversity/censorship-intensity data across confessional territories into the 1700s.',
    'An earlier or later sunset date shifts how much of the measured extraction/suppression trajectory should be attributed to scaffold operation versus post-scaffold piton persistence (theatrical licensing enforcement after the coordination need passed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_sunset_dating, empirical, 'Uncertainty about when the scaffold''s transitional function actually ended.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__co_constitution, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causality__co_constitution, theater_ratio, 1490, 0.08).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__co_constitution, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causality__co_constitution, theater_ratio, 1530, 0.16).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__co_constitution, theater_ratio, 1555, 0.2).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__co_constitution, theater_ratio, 1600, 0.24).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__co_constitution, theater_ratio, 1650, 0.22).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__co_constitution, base_extractiveness, 1450, 0.18).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causality__co_constitution, base_extractiveness, 1490, 0.22).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__co_constitution, base_extractiveness, 1517, 0.3).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causality__co_constitution, base_extractiveness, 1530, 0.4).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__co_constitution, base_extractiveness, 1555, 0.44).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__co_constitution, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__co_constitution, base_extractiveness, 1650, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__co_constitution, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement(pres_su_t1490, press_reformation_causality__co_constitution, suppression_requirement, 1490, 0.16).
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__co_constitution, suppression_requirement, 1517, 0.28).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causality__co_constitution, suppression_requirement, 1530, 0.48).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__co_constitution, suppression_requirement, 1555, 0.55).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__co_constitution, suppression_requirement, 1600, 0.5).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causality__co_constitution, suppression_requirement, 1650, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__co_constitution, 0.05).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causality kernel. technological_determinism authors the press as an autonomous enabling mountain/scaffold whose spread effects follow from its inherent replication capacities; strategic_deployment authors reformers and printers as strategic agents instrumentalizing an inert technology toward pre-formed goals; this co_constitution reading authors the causality as an irreducible feedback loop between technological capacity and human agency, yielding distributed tangled_rope dynamics rather than a single beneficiary chain. All three share the same underlying historical episode but differ in where they locate causal primacy, which produces different beneficiary/victim structures and different classifications for the printer, reformer, and Church seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
