% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Print-Mediated Diffusion of Reform Doctrine (Technological Mediation Reading)
 *   domain: historical epistemology / religious history / political economy
 *
 * SUMMARY:
 *   This story instantiates the technological-mediation reading of the
 *   Reformation kernel: the causal engine of the continental scale and speed
 *   of reform diffusion is the physical/logical properties of movable-type
 *   printing itself, not the theological content it carried or the political
 *   sovereignty claims it enabled (those are separate sibling
 *   readings/constraints). The press is the mountain-like substrate — its
 *   reproduction physics (identical copies, low marginal cost, geographic
 *   mobility of type-cases) is fixed and would have enabled rapid diffusion
 *   of any sufficiently controversial content, not only Luther's. Layered on
 *   top of that substrate is a tangled-rope coordination/extraction
 *   structure: printers and reformers coordinate genuinely (a real problem —
 *   slow, unreliable, expensive text reproduction — gets solved), but the
 *   same structure asymmetrically extracts from populations who could not
 *   read the texts driving the conflict and who bore its costs (war,
 *   confiscation, communal rupture) without control over its terms, and from
 *   manuscript-economy clergy whose vocation is rendered obsolete without
 *   transition support.
 *
 * KEY AGENTS:
 *   - printer_publishers: commercial owners of press capital, mobile beneficiaries
 *   - vernacular_reformers: theological content producers whose reach the technology extends
 *   - illiterate_rural_peasantry: bear conflict costs without textual access
 *   - manuscript_scriptoria_clergy: obsoleted incumbents with no transition exit
 *   - territorial_princes_and_city_councils: agenda-setters whose licensing choices determine local outcome variance
 *   - historians_of_print_culture: analytical observers assessing the print-causation thesis itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.42).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.55).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Print-Mediated Diffusion of Reform Doctrine (Technological Mediation Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical epistemology / religious history / political economy").

domain_priors:requires_active_enforcement(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'd1c2517a-4256-4049-a9af-bd67a79d6fb4').
narrative_ontology:cs_kernel_codification('d1c2517a-4256-4049-a9af-bd67a79d6fb4', distributed).
narrative_ontology:cs_authority_grounding('d1c2517a-4256-4049-a9af-bd67a79d6fb4', distributed).
narrative_ontology:cs_reading_relation('d1c2517a-4256-4049-a9af-bd67a79d6fb4', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('d1c2517a-4256-4049-a9af-bd67a79d6fb4', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('d1c2517a-4256-4049-a9af-bd67a79d6fb4', foundational, reproduction_technology_is_primary_causal_substrate).
narrative_ontology:cs_axiom_status(reproduction_technology_is_primary_causal_substrate, holdable).
narrative_ontology:cs_axiom_grounding('d1c2517a-4256-4049-a9af-bd67a79d6fb4', reproduction_technology_is_primary_causal_substrate, empirically_contingent).
narrative_ontology:cs_axiom('d1c2517a-4256-4049-a9af-bd67a79d6fb4', secondary, content_neutral_diffusion_mechanism).
narrative_ontology:cs_axiom_status(content_neutral_diffusion_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('d1c2517a-4256-4049-a9af-bd67a79d6fb4', content_neutral_diffusion_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('d1c2517a-4256-4049-a9af-bd67a79d6fb4', manuscript_era_reproduction_monopoly).
narrative_ontology:cs_drift_state('d1c2517a-4256-4049-a9af-bd67a79d6fb4', post_1517_pamphlet_surge, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('d1c2517a-4256-4049-a9af-bd67a79d6fb4', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, printer_publishers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, vernacular_reformers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_urban_burghers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, illiterate_rural_peasantry).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, manuscript_scriptoria_clergy).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, suppressed_dissenting_printers).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, print_capitalism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own presses and typesetting capital; choose which pamphlets, broadsides, and translated scripture to run based on what sells fastest across urban markets. They capture the commercial upside of theological controversy — a Luther pamphlet sold out editions in days — and can relocate operations to friendlier cities (Basel, Strasbourg, Wittenberg) if local authorities crack down. Their choices determine which reform arguments reach mass circulation and which do not.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printer_publishers, beneficiary,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, printer_publishers, agenda_setter).

% Theologians and preachers whose arguments, once typeset, escape the reach of any single bishop or prince. The press converts a local dispute into a portable, replicable text that travels faster than ecclesiastical censure can follow. Their exit options are limited to fleeing to print-friendly territories, but the technology itself gives their ideas an exit their bodies do not have.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, vernacular_reformers, beneficiary,
    moderate, biographical, constrained, continental).

% Merchants, guild members, and municipal officials with enough literacy to consume pamphlets directly. They gain access to theological argument previously mediated only through clergy, and use this access to reshape civic religious policy in their own cities.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_urban_burghers, beneficiary,
    moderate, generational, mobile, regional).

% Cannot read the pamphlets directly and receive reform doctrine only through oral relay, sermon, or woodcut image, often stripped of nuance or weaponized by local elites into justification for uprising (as in the Peasants' War) or suppression. They bear the costs of doctrinal conflict — violence, confiscation, war taxation — without controlling the terms of the argument that produces it.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_rural_peasantry, payer,
    powerless, biographical, trapped, local).

% Monastic copyists and manuscript-based clerical scholars whose institutional function — producing and controlling scripture and commentary by hand — is rendered economically obsolete within a generation. They cannot compete with press output on cost or speed and have no exit into the new technological economy without abandoning their vocation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, manuscript_scriptoria_clergy, payer,
    moderate, biographical, trapped, regional).

% Printers in territories where authorities successfully identify and punish heterodox presses — seized type, burned stock, execution in extreme cases. Unlike the mobile printer-publishers who relocate ahead of enforcement, these operators are caught inside jurisdictions with effective censorship apparatus and bear the full penalty for the same commercial choice that rewards their more mobile counterparts.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, suppressed_dissenting_printers, payer,
    powerless, immediate, trapped, local).

% Grant or deny printing licenses, decide whether to tolerate or suppress reform presses within their jurisdiction, and use the resulting religious realignment to bargain with imperial and papal authority. Their enforcement decisions are what make the press's effects locally variable — the same technology produces reform triumph in Wittenberg and suppression in other territories depending on this seat's choice.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, territorial_princes_and_city_councils, agenda_setter,
    institutional, generational, constrained, regional).

% Attempt to control the new medium through indices of forbidden books and licensing decrees, but face a fundamentally different speed and geography of production than the manuscript economy they were built to police. Their institutional response lags the technology structurally, not merely tactically — by the time a text is indexed, dozens of editions already circulate in territories beyond their writ.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, papal_and_imperial_censors, excluded,
    institutional, generational, constrained, continental).

% Study publication counts, edition sizes, and literacy correlation data to assess how much of the Reformation's spread is attributable to the press itself versus theological content, political sponsorship, or social grievance that would have found other outlets.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, historians_of_print_culture, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Movable-type printing solves a genuine coordination problem: it allows a single argued position to be reproduced identically and rapidly across many locations, letting geographically dispersed readers coordinate around a shared text rather than relying on unreliable oral transmission or scarce, error-prone manuscript copies.
% TRANSFER_FUNCTION: Moves argumentative and doctrinal authority away from scriptoria, bishops, and oral catechesis and toward printers and literate readers; moves commercial profit from manuscript production to press ownership; moves the cost of doctrinal conflict onto populations who could not read the texts driving it but bore the resulting violence and jurisdictional realignment.
% ABSENT_VOICES: The illiterate rural peasantry and lower clergy who experienced the Reformation's consequences (war, confiscation, doctrinal upheaval) almost entirely through intermediaries are structurally absent from the textual record this reading privileges — publication counts and literacy rates measure the literate stratum's experience and are silent on everyone else's.
% DISAPPEARANCE_RATIONALE: Without movable-type printing, Luther's arguments would have circulated at Hussite-era speed and reach — a local or regional dispute suppressible by conventional ecclesiastical and political means, as prior reform movements were. The specific continental, near-simultaneous, multi-territory character of the Reformation depends on a reproduction technology capable of outrunning manuscript-era censorship; remove it and the theological content very plausibly remains a contained heresy rather than a continental rupture.
% FOUNDING_PROBLEM: The press itself was not built to cause religious schism — it was built to solve a reproduction-cost and reproduction-speed problem in text production generally (commercial, administrative, and religious). Its application to reform pamphleteering was an emergent use, not a designed function.
% FOUNDING_PROBLEM_CORROBORATION: Print historians (Eisenstein, Febvre and Martin, and successors debating the 'printing revolution' thesis) attest from outside any confessional beneficiary group that the press's generic reproduction function, not any theological content, is what changed the dynamics of dissent; this is corroborated by comparable press-enabled diffusion in wholly non-religious domains (scientific, legal, commercial texts) in the same period, which no beneficiary of Reformation doctrine would have reason to assert.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).
:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at plateau) rather than high because a genuine coordination function (faster, cheaper, more reliable text reproduction) is real and widely shared, not a pure cover story; the extraction component is the asymmetric burden placed on illiterate populations and obsoleted manuscript-clergy who did not choose the technology's application to religious controversy and could not exit it. Suppression rises sharply after 1517 (0.1 to 0.4 to 0.55) tracking the historical emergence and intensification of licensing regimes, indices of forbidden books, and press seizures as authorities recognized the technology's evasion of manuscript-era censorship geography — this is an enforcement-intensification pattern, not a stable baseline, which is why suppression_requirement is tracked temporally rather than left as a flat scalar. Theater ratio stays low-to-moderate (0.05 to 0.2) because censorship efforts, while increasingly performative in outrunning actual print volume, retained substantial genuine enforcement teeth (executions, stock seizures) throughout the period.
 *
 * DIRECTIONALITY LOGIC:
 *   Printer-publishers and vernacular reformers sit near the beneficiary end: they capture commercial and argumentative advantage and retain meaningful exit (relocating presses, fleeing to friendlier territories). Illiterate rural peasantry and trapped local printers sit near the full-target end: no literacy-based access to the content driving conflict, no exit from the territories where consequences land. Manuscript clergy are a distinct victim class — not targeted by enforcement, but structurally obsoleted with no available transition, which the framework treats as a payer role driven by asymmetric technological displacement rather than coercive extraction per se.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reproduction cost/speed of text generally) is dead as a live justification for any of the printing industry's later confessional or commercial entrenchments by 1600 — the press had long since generalized far beyond religious controversy into legal, scientific, and commercial print. Framing the ongoing operation of print capitalism in confessional terms after this point would be mandatrophy; this story's interval closes before that drift becomes acute, but the corroboration trail (print historians outside any confessional stake) supports treating the technological substrate as functionally separate from the doctrinal content it happened to carry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    print_as_necessary_vs_sufficient_cause,
    'Is the printing press a necessary condition for the Reformation''s continental scale (without it, reform stays regional, as with prior heresies), or merely an accelerant of a rupture that social, political, and theological pressures would eventually have produced by other means?',
    'Comparative analysis against pre-print heterodox movements (Hussites, Lollards, Waldensians) that achieved regional but not continental scale, controlling for the theological content''s intrinsic appeal and the political conditions of the receiving territories.',
    'If necessary, the technological-mediation reading''s causal primacy is strongly supported and the press functions as a true structural precondition (mountain) beneath the whole event. If merely accelerant, this reading''s claim to primacy over the theological and political readings weakens considerably, and technological mediation becomes one input among several co-equal causes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(print_as_necessary_vs_sufficient_cause, empirical, 'Whether print technology was necessary or merely accelerating for Reformation-scale diffusion.').

omega_variable(
    literacy_proxy_validity,
    'Do publication rates and literacy figures accurately proxy the causal reach of the press, given that oral relay, sermon, and image (woodcuts) carried print-originated content to illiterate populations who never touched a text directly?',
    'Social history reconstruction of oral and visual transmission chains from printed originals, comparing outcome patterns in high-literacy versus low-literacy regions.',
    'If oral/visual relay substantially closes the literacy gap, the extraction borne by illiterate populations in this story is overstated relative to their actual exposure to content, and the victim classification for illiterate_rural_peasantry may need refinement toward a more symmetric directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_proxy_validity, empirical, 'Whether print-derived literacy metrics adequately capture the technology''s actual reach into illiterate populations.').

omega_variable(
    kernel_reading_independence,
    'Are the technological, theological, and political readings of the Reformation kernel genuinely independent causal claims, or does the technological-mediation reading implicitly presuppose theological content worth spreading and political conditions permitting its spread — making the readings nested rather than parallel?',
    'Formal counterfactual decomposition: hold press technology constant and vary theological/political content across cases, and vice versa, checking whether continental-scale diffusion tracks each variable independently.',
    'If nested rather than parallel, the three-story decomposition of this kernel may need a fourth story capturing the interaction term, rather than treating the readings as fully separable per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_independence, conceptual, 'Whether the kernel''s three readings are structurally independent or interdependent causal claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1450, reformation_composite__technological_mediation_reading, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(refo_tr_t1480, reformation_composite__technological_mediation_reading, theater_ratio, 1480, 0.08).
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__technological_mediation_reading, theater_ratio, 1517, 0.12).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__technological_mediation_reading, theater_ratio, 1530, 0.18).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__technological_mediation_reading, theater_ratio, 1555, 0.2).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__technological_mediation_reading, theater_ratio, 1600, 0.2).

% Extraction over time
narrative_ontology:measurement(refo_be_t1450, reformation_composite__technological_mediation_reading, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(refo_be_t1480, reformation_composite__technological_mediation_reading, base_extractiveness, 1480, 0.15).
narrative_ontology:measurement(refo_be_t1517, reformation_composite__technological_mediation_reading, base_extractiveness, 1517, 0.3).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__technological_mediation_reading, base_extractiveness, 1530, 0.4).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__technological_mediation_reading, base_extractiveness, 1555, 0.42).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__technological_mediation_reading, base_extractiveness, 1600, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1450, reformation_composite__technological_mediation_reading, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement(refo_su_t1480, reformation_composite__technological_mediation_reading, suppression_requirement, 1480, 0.15).
narrative_ontology:measurement(refo_su_t1517, reformation_composite__technological_mediation_reading, suppression_requirement, 1517, 0.4).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__technological_mediation_reading, suppression_requirement, 1530, 0.55).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__technological_mediation_reading, suppression_requirement, 1555, 0.55).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__technological_mediation_reading, suppression_requirement, 1600, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_composite__technological_mediation_reading, 0.05).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, political_realignment_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the 'Reformation' kernel per the ε-invariance principle. Each sibling authors its own ε from a distinct causal-primacy claim: this story (technological_mediation_reading) treats print reproduction physics as the generative substrate (ε=0.42, tangled_rope — moderate extraction riding a genuine coordination function); theological_fragmentation_reading treats incompatible soteriological/ecclesiological commitments as generative; political_realignment_reading treats nation-state sovereignty assertion as generative. The three do not average into one 'Reformation' ε — they are linked via affects_constraints because the technological substrate plausibly conditions the scale at which theological and political dynamics could operate (a printing-enabled theological dispute reaches further than a manuscript-bound one), but each retains its own independent stakeholder set, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
