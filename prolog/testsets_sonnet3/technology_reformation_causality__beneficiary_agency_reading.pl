% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__beneficiary_agency_reading, []).

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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition as Strategic Authority-Bypass Instrument
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Reformist clergy, commercial printers, and sympathetic territorial
 *   princes formed a functioning if uncoordinated coalition: reformers
 *   supplied doctrinal content and rhetorical urgency, printers supplied
 *   capital, technical capacity, and profit motive, and princes supplied
 *   territorial protection and enforcement withdrawal against Church
 *   sanctions. Together they produced a distribution machine that outpaced
 *   Church censorship faster than any single actor could have alone. The
 *   claimed type (tangled_rope) captures both a genuine coordination function
 *   — solving each party's distinct problem simultaneously — and asymmetric
 *   extraction, where lay readers, unlicensed competitors, and local clergy
 *   bore costs the coalition did not internalize.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.66).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.58).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition as Strategic Authority-Bypass Instrument").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '3cae0287-cf49-47e0-94fc-0edd19d38568').
narrative_ontology:cs_kernel_codification('3cae0287-cf49-47e0-94fc-0edd19d38568', distributed).
narrative_ontology:cs_authority_grounding('3cae0287-cf49-47e0-94fc-0edd19d38568', practice).
narrative_ontology:cs_interpretation_layer_present('3cae0287-cf49-47e0-94fc-0edd19d38568').
narrative_ontology:cs_reading_relation('3cae0287-cf49-47e0-94fc-0edd19d38568', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cae0287-cf49-47e0-94fc-0edd19d38568', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('3cae0287-cf49-47e0-94fc-0edd19d38568', foundational, technology_as_instrument_not_cause).
narrative_ontology:cs_axiom_status(technology_as_instrument_not_cause, holdable).
narrative_ontology:cs_axiom_grounding('3cae0287-cf49-47e0-94fc-0edd19d38568', technology_as_instrument_not_cause, empirically_contingent).
narrative_ontology:cs_axiom('3cae0287-cf49-47e0-94fc-0edd19d38568', foundational, reformer_printer_coalition_deliberate_strategy).
narrative_ontology:cs_axiom_status(reformer_printer_coalition_deliberate_strategy, holdable).
narrative_ontology:cs_axiom_grounding('3cae0287-cf49-47e0-94fc-0edd19d38568', reformer_printer_coalition_deliberate_strategy, empirically_contingent).
narrative_ontology:cs_reference_frame('3cae0287-cf49-47e0-94fc-0edd19d38568', reformer_printer_strategic_deployment_thesis).
narrative_ontology:cs_drift_state('3cae0287-cf49-47e0-94fc-0edd19d38568', contemporary_historiographical_synthesis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3cae0287-cf49-47e0-94fc-0edd19d38568', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, protestant_reformer_leadership).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, commercial_printer_guilds).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, allied_territorial_princes).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, lay_readers_and_congregants).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, unlicensed_pamphlet_printers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, catholic_clergy_local).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, lay_readers_and_congregants).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, reformer_agency_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, technology_as_instrument_not_cause).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selects which texts get printed, negotiates with printers and sympathetic princes, times pamphlet releases to maximize disruption of Church authority, and curates doctrine for mass circulation. Builds personal and institutional authority on the resulting readership and political backing this strategy generates.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, protestant_reformer_leadership, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, protestant_reformer_leadership, beneficiary).

% Runs presses as a business, chooses reformist pamphlets because they sell reliably and repeatedly, relocates operations across jurisdictional lines when local authorities threaten seizure, and profits directly from the demand the reformers' campaign creates.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, commercial_printer_guilds, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, commercial_printer_guilds, agenda_setter).

% Grants protection and printing privileges to reformist presses within their territories in exchange for weakening papal and imperial claims on their subjects and revenue; extracts political sovereignty gains from the coalition's output.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, allied_territorial_princes, beneficiary,
    institutional, generational, arbitrage, regional).

% Receives vernacular scripture and polemic through the new print channel, gains some access to text previously mediated by clergy, but is also targeted by carefully engineered propaganda, drawn into confessional conflict and communal violence, and given little say in which texts circulate or how doctrine is framed for them.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, lay_readers_and_congregants, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, lay_readers_and_congregants, beneficiary).

% Operates smaller presses outside the reformer-endorsed networks, gets undercut, denounced as heretical or seditious by whichever authority currently holds local power, and lacks the protection deals the leading coalition presses enjoy.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, unlicensed_pamphlet_printers, payer,
    moderate, immediate, trapped, local).

% Loses congregational trust and revenue as reformist print floods a parish; cannot match the coalition's distribution speed or its alliance with sympathetic printers and princes; bears reputational and material cost of the bypass strategy without having engineered it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_clergy_local, payer,
    moderate, biographical, trapped, regional).

% Assesses competing causal narratives about the press's role, weighing evidence for deliberate strategic deployment against claims of technological inevitability or mutual co-shaping.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, protestant_reformer_leadership).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reformers and printers coordinate to produce and circulate vernacular religious material at a speed and scale that outpaces Church licensing and censorship control, solving each party's problem simultaneously — doctrinal reach for reformers, reliable sales for printers, sovereignty leverage for allied princes.
% TRANSFER_FUNCTION: Moves interpretive authority and congregational loyalty away from the institutional Church and toward reformist leadership and their printer-prince allies; moves commercial profit to the presses; moves political leverage to sympathetic territories; moves the costs of confessional conflict, doctrinal confusion, and reputational loss onto lay readers, unlicensed competitors, and local clergy who did not choose the strategy.
% ABSENT_VOICES: Lay readers had no seat in deciding which texts, translations, or interpretive framings were mass-produced for them; unlicensed printers who lacked coalition protection were prosecuted or ruined without recourse; local clergy bearing the reputational cost of the bypass had no forum to contest the strategy that displaced them.
% DISAPPEARANCE_RATIONALE: If the reformer-printer-prince coalition had not formed or acted deliberately, print technology alone (absent strategic deployment) would not have produced the same rapid, targeted erosion of Church authority — distribution networks, alliances, and content curation would have to be rebuilt by some other coordinating agents, and the pace and shape of confessional conflict would differ substantially.
% FOUNDING_PROBLEM: Reform-minded clergy and scholars needed to circulate vernacular scripture and polemical argument fast enough, and widely enough, to outrun Church censorship, excommunication proceedings, and local suppression before their movement could be contained.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of print culture and Reformation-era legal records (episcopal court proceedings against printers, imperial edicts against unlicensed presses) corroborate that the original suppression threat has long since ended; however, denominational historical accounts produced within Protestant institutional traditions themselves continue to narrate the strategy as ongoing vindication of reform legitimacy rather than as a closed historical episode — the corroborating outside sources are legal and bibliographic archives, not the confessional histories.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.38 to 0.66) as the coalition's methods matured from ad hoc pamphleteering into organized, protected, profit-driven print networks — the authority-bypass value increased as the coalition learned to coordinate content, capital, and protection more effectively. Suppression tracks a parallel rise (0.30 to 0.58) as the coalition itself began enforcing orthodoxy within its own movement (denouncing unlicensed or heterodox printers, controlling which vernacular translations counted as legitimate) even as it undermined Church suppression. Theater ratio rises moderately (0.20 to 0.42) reflecting growing performative doctrinal purity contests that served coalition cohesion more than genuine theological resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer-printer-prince coalition's own seat, this looks like principled, strategic resistance to illegitimate authority — a rope solving a genuine coordination problem. From the seat of lay readers, unlicensed printers, and displaced clergy, the same structure operated as an extractive machine that used them as audience, competition, or collateral damage while concentrating the gains of authority-bypass among the coalition's core members. The engine should compute divergent per-seat classifications from these structural declarations rather than adjudicating a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformer leadership, commercial printer guilds, and allied princes are declared beneficiaries because each captured a distinct, concrete gain — doctrinal reach, commercial profit, and political sovereignty respectively — directly from the coalition's operation; their exit options are mobile or arbitrage-grade, letting them relocate or renegotiate across jurisdictions as pressure shifted. Lay readers, unlicensed printers, and local clergy are declared victims/payers because they bore the coalition's externalized costs (propaganda targeting, competitive exclusion, reputational and material loss) without comparable exit options — congregants were geographically and socially bound to their parishes, unlicensed printers lacked the protective deals of the coalition's core, and local clergy could not relocate their pastoral obligations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — outrunning Church censorship before movements could be suppressed — is now dead; the coalition-descended religious and print institutions persist, but the acute suppression threat that justified the original bypass strategy ended centuries ago. Confessional historiography that continues to narrate the coalition's actions purely as heroic agency (vindicating the reformer_agency_thesis) risks obscuring the asymmetric extraction the coalition also practiced against groups it did not represent, which is precisely the mandatrophy this classification is built to surface without denying the coalition's real coordination achievement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition_boundary,
    'Where exactly does the deliberate-agency component of the Reformation-print relationship end and the technology''s own enabling/constraining properties begin — is the partition between this reading and the determinism/co-constitution siblings a matter of historical evidence or of interpretive framework choice?',
    'Comparative case studies isolating regions/periods where equivalent print technology existed without comparable reformist coalition organization, versus regions with strong coalition organization but weaker print infrastructure, to see whether outcomes track agency or technological capacity.',
    'If outcomes track coalition organization more than raw technological availability, this reading''s causal claim strengthens relative to determinism; if outcomes track technological penetration regardless of coalition strength, the determinism reading gains support and this reading''s ε attribution to ''strategic deployment'' would be partly misattributed to technology itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_partition_boundary, conceptual, 'Whether the agency/technology partition in this kernel is empirically decidable or an artifact of framing choice.').

omega_variable(
    coalition_internal_asymmetry,
    'Was the reformer-printer-prince coalition a genuine tripartite alliance with roughly comparable bargaining power, or did one party (most likely reformer leadership or allied princes) structurally dominate the terms, making ''coalition'' itself a beneficiary-serving euphemism for a more hierarchical arrangement?',
    'Archival analysis of printing privilege contracts, patronage records, and correspondence to assess actual bargaining leverage and revenue/benefit distribution among the three declared beneficiary groups.',
    'If one party structurally dominated, the tangled_rope classification''s ''coordination function'' framing may itself need revision toward a more snare-like reading for the subordinate beneficiary groups, and the beneficiary set''s internal directionality would need finer differentiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_internal_asymmetry, empirical, 'Whether the declared coalition beneficiaries held comparable or asymmetric power within the arrangement.').

omega_variable(
    lay_reader_net_position,
    'Did lay readers and congregants net-benefit from expanded vernacular scripture access despite bearing propaganda and conflict costs, making their ''payer'' classification too one-sided?',
    'Social historical study comparing literacy rates, self-reported religious agency, and material welfare of lay populations before and after coalition print campaigns, weighted against documented instances of confessional violence and displacement they experienced.',
    'If net benefit dominates, lay readers'' directionality should shift closer to symmetric rather than payer-leaning, softening the tangled_rope''s victim-side claim; if costs dominate, the current payer classification and tangled_rope reading are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_reader_net_position, empirical, 'Whether lay readers were net beneficiaries or net payers of the coalition''s strategic print deployment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1517, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(tech_tr_t1522, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1522, 0.26).
narrative_ontology:measurement(tech_tr_t1529, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1529, 0.32).
narrative_ontology:measurement(tech_tr_t1536, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1536, 0.36).
narrative_ontology:measurement(tech_tr_t1546, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1546, 0.4).
narrative_ontology:measurement(tech_tr_t1555, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1555, 0.42).

% Extraction over time
narrative_ontology:measurement(tech_be_t1517, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1517, 0.38).
narrative_ontology:measurement(tech_be_t1522, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1522, 0.47).
narrative_ontology:measurement(tech_be_t1529, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1529, 0.55).
narrative_ontology:measurement(tech_be_t1536, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1536, 0.6).
narrative_ontology:measurement(tech_be_t1546, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1546, 0.64).
narrative_ontology:measurement(tech_be_t1555, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1555, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1517, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(tech_su_t1522, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1522, 0.4).
narrative_ontology:measurement(tech_su_t1529, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1529, 0.49).
narrative_ontology:measurement(tech_su_t1536, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1536, 0.53).
narrative_ontology:measurement(tech_su_t1546, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1546, 0.56).
narrative_ontology:measurement(tech_su_t1555, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1555, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, identity_coordination).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the technology_reformation_causality kernel. technological_determinism_reading treats print technology as the primary causal driver making the Reformation structurally inevitable (likely mountain-leaning, low agency attribution). co_constitution_reading treats technology and reformist actors as mutually and iteratively shaping each other (likely a hybrid classification blending scaffold and rope elements). This reading (beneficiary_agency_reading) treats the coalition's deliberate strategic deployment as the primary explanatory and extractive mechanism, classified as tangled_rope with technology itself functioning as a scaffold — a transitional instrument the coalition used and eventually institutionalized past its original bypass purpose. Each sibling authors its own ε; they are not to be averaged or reconciled into one value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
