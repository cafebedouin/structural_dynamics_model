% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Weaponization of the Printing Press Against Ecclesiastical Authority
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the strategic_deployment reading of the
 *   press_reformation_causality kernel: the printing press treated not as an
 *   autonomous causal force (technological_determinism) nor as a co-evolving
 *   system with human agency in feedback (co_constitution), but as an
 *   instrument that specific, identifiable actors — Luther and allied
 *   reformist clergy, commercially motivated printers in Wittenberg and
 *   Basel, and territorial princes seeking to seize church revenue —
 *   deliberately selected and optimized against a specific target: Roman
 *   ecclesiastical authority and its local revenue apparatus (indulgence
 *   sellers, the Latin-literate clerical monopoly on scriptural
 *   interpretation). The press here is the weapon, not the cause; the causal
 *   work is done by the strategic choices of the wielders. Coordination
 *   (rope-like: reaching scattered sympathizers with synchronized vernacular
 *   text) is real and is what makes this readable as a rope-flavored tool,
 *   but the constraint as authored — the press specifically deployed as
 *   extraction against the Curia's authority and revenue — is a snare from
 *   the target's structural position: victims (Curia, indulgence sellers,
 *   clerical monopoly) had no meaningful counter-technology in the relevant
 *   timeframe and suffered concentrated, deliberate loss.
 *
 * KEY AGENTS:
 *   - reformist_clergy: primary strategic agent (organized/mobile) — commissions and times print output for maximal disruptive effect
 *   - printer_guilds: commercial beneficiary (organized/mobile) — supplies the weapon for profit, shares in the extraction
 *   - territorial_princes: institutional beneficiary (institutional/arbitrage) — converts religious rupture into territorial and fiscal gain
 *   - roman_curia: primary target (institutional/constrained) — bears the loss of revenue and interpretive authority
 *   - indulgence_sellers: concentrated local victim (moderate/trapped) — reputational and economic destruction via targeted print satire
 *   - latin_literate_clerical_monopoly: structural victim (powerful/constrained) — the specific advantage the strategy targets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.68).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.62).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, snare).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Weaponization of the Printing Press Against Ecclesiastical Authority").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, '5d7c6184-c625-421e-bcb9-51090cdc7df6').
narrative_ontology:cs_kernel_codification('5d7c6184-c625-421e-bcb9-51090cdc7df6', distributed).
narrative_ontology:cs_authority_grounding('5d7c6184-c625-421e-bcb9-51090cdc7df6', extraction).
narrative_ontology:cs_interpretation_layer_present('5d7c6184-c625-421e-bcb9-51090cdc7df6').
narrative_ontology:cs_reading_relation('5d7c6184-c625-421e-bcb9-51090cdc7df6', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('5d7c6184-c625-421e-bcb9-51090cdc7df6', press_reformation_causality__co_constitution, influences).
narrative_ontology:cs_axiom('5d7c6184-c625-421e-bcb9-51090cdc7df6', foundational, agency_locates_in_strategic_actors_not_medium).
narrative_ontology:cs_axiom_status(agency_locates_in_strategic_actors_not_medium, holdable).
narrative_ontology:cs_axiom_grounding('5d7c6184-c625-421e-bcb9-51090cdc7df6', agency_locates_in_strategic_actors_not_medium, empirically_contingent).
narrative_ontology:cs_axiom('5d7c6184-c625-421e-bcb9-51090cdc7df6', secondary, press_deployment_was_targeted_extraction_not_neutral_diffusion).
narrative_ontology:cs_axiom_status(press_deployment_was_targeted_extraction_not_neutral_diffusion, holdable).
narrative_ontology:cs_axiom_grounding('5d7c6184-c625-421e-bcb9-51090cdc7df6', press_deployment_was_targeted_extraction_not_neutral_diffusion, empirically_contingent).
narrative_ontology:cs_reference_frame('5d7c6184-c625-421e-bcb9-51090cdc7df6', papal_doctrinal_monopoly_via_manuscript_control).
narrative_ontology:cs_drift_state('5d7c6184-c625-421e-bcb9-51090cdc7df6', post_ninety_five_theses_pamphlet_wave, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5d7c6184-c625-421e-bcb9-51090cdc7df6', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformist_clergy).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printer_guilds).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, territorial_princes).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, roman_curia).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, indulgence_sellers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, latin_literate_clerical_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, lay_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Figures like Luther and his allies deliberately commissioned pamphlets, translated scripture into vernacular languages, and coordinated print runs timed to controversies (the 95 Theses printed and distributed within weeks). They chose print over disputation precisely because it bypassed clerical gatekeeping and reached lay audiences directly, converting a theological argument into a mass-distributed political fact.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformist_clergy, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, reformist_clergy, beneficiary).

% Print workshops in Wittenberg, Basel, and Strasbourg found a lucrative, high-volume market in reformist tracts — cheaper to produce than Latin theological folios and selling in the thousands. Printers actively solicited reformist authors, extended credit against expected sales, and competed to be first to market with inflammatory pamphlets, treating religious controversy as a commercial opportunity.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printer_guilds, beneficiary,
    organized, biographical, mobile, continental).

% German princes who protected reformers and licensed reformist presses gained leverage to seize church lands, end payments to Rome, and consolidate authority over territorial churches. They used the printed word strategically to legitimate confiscation and rally popular support against imperial and papal claims on their territory.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, territorial_princes, beneficiary,
    institutional, generational, arbitrage, national).

% The papacy and its administrative apparatus lost revenue, doctrinal authority, and control of the narrative as printed attacks outpaced any response capacity — bulls and condemnations issued in Latin manuscript-era timeframes while vernacular pamphlets multiplied weekly. Rome's traditional tools (excommunication, controlled manuscript circulation) had no answer to a technology optimized against them by opponents who understood its speed and reach.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, roman_curia, payer,
    institutional, civilizational, constrained, continental).

% Commissioners like Tetzel became the specific named targets of printed satire and polemic, their livelihoods and local credibility destroyed by pamphlets that could be reproduced and distributed faster than any personal reputation could be defended. Their trade depended on local trust relationships that print-driven mass mockery could dissolve in a season.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, indulgence_sellers, payer,
    moderate, biographical, trapped, regional).

% The educated clergy's monopoly on scriptural interpretation, previously secured by Latin literacy and manuscript scarcity, was deliberately targeted by reformers' vernacular translation strategy. Their structural advantage — controlling who could read and interpret scripture — was the specific thing reformist printers set out to dissolve, not an incidental casualty.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, latin_literate_clerical_monopoly, payer,
    powerful, generational, constrained, continental).

% Newly literate or read-to laypeople gained direct access to vernacular scripture and religious argument for the first time, but their access was shaped entirely by what reformers and printers chose to produce and distribute — a curated flood of one side's material, not a neutral information environment.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, lay_readers, beneficiary,
    powerless, biographical, constrained, regional).

% Catholic-aligned printers who might have countered reformist output faced a structural disadvantage: the printing centers most receptive to reformist material had already tied their commercial fortunes to that market, and Rome's slower, more centralized approval processes for orthodox texts could not match the improvisational speed of reformist pamphleteering.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, counter_reformation_printers, excluded,
    organized, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Print coordinated distributed audiences of readers and listeners around a shared vernacular text and a shared timing of controversy, letting geographically scattered sympathizers act on the same information at nearly the same moment — a genuine solution to the coordination problem of building a mass movement without physical assembly.
% TRANSFER_FUNCTION: Moves religious authority, revenue (tithes, indulgence income, benefice fees), and interpretive legitimacy from the Roman ecclesiastical hierarchy and its local agents to reformist clergy, allied printers, and the territorial princes who backed them.
% ABSENT_VOICES: Counter-Reformation printers and the Roman Curia's own communications apparatus are structurally outpaced, not silenced by rule — they are excluded from the tempo of the exchange, not from participation itself, and their objection (that this is deliberate propaganda warfare, not a neutral technology) is a matter of documented record on their side, not an absent argument.
% DISAPPEARANCE_RATIONALE: If the strategic print campaigns disappeared but the press itself remained available for other uses, the specific timing, coordination, and mass distribution that let a local doctrinal dispute become a continental political rupture within a decade would not have occurred in the same form — the Reformation's trajectory, pace, and territorial outcomes depended on deliberate exploitation of print's coordination properties by specific actors with specific goals.
% FOUNDING_PROBLEM: Reformers needed to break the Roman Curia's control over scriptural interpretation and mobilize lay and princely support faster than ecclesiastical or imperial authority could respond or suppress the challenge.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Catholic polemicists (e.g., Johann Eck, Thomas More) attested at the time that reformist print use was a deliberate propaganda strategy, not incidental technology diffusion — corroboration from outside the beneficiary set. Modern print-culture historians (Eisenstein's critics and defenders alike) document printer account books and reformist correspondence showing calculated commissioning of print runs, which is independent archival corroboration beyond either side's self-narrative.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.35 to 0.68) reflecting the escalation from Luther's initial theses to systematic, industrialized pamphlet warfare by the Schmalkaldic period — the strategy matures and its extraction from Rome's authority and revenue base compounds. Suppression is authored moderate-high (0.62) because the strategic deployment reading holds that reformers and princes actively worked to suppress Rome's capacity to respond (seizing printing privileges, denying orthodox presses market access in reformed territories) rather than merely outcompeting a neutral alternative. Theater ratio stays low-moderate (0.28) because the propaganda function was substantively real, not performative — pamphlets did carry doctrinal content that mattered to readers, even as they were also weapons.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist clergy, printers, and princes are declared beneficiaries with mobile/arbitrage exit — they chose the strategy, profited from it, and could adjust tactics as circumstances changed. The Curia, indulgence sellers, and the clerical interpretive monopoly are declared victims with constrained/trapped exit — they had no equivalent print-speed counter-technology available in the critical early years and bore concentrated, targeted loss of revenue and authority. Lay readers are beneficiaries of access but with constrained exit, since what they received was curated by the strategic actors rather than a neutral information commons — this qualifies their beneficiary status without erasing it.
 *
 * MANDATROPHY ANALYSIS:
 *   The strategic_deployment reading resists the trap of treating the Reformation's print explosion as either pure inevitable technological diffusion (which would erase the agency and moral responsibility of reformers and printers who made deliberate targeting choices) or pure organic coordination (which would erase the real, documented economic and political losses inflicted on specific named victims). By authoring this as a snare-with-coordination-substrate rather than a pure rope, the classification preserves both the genuine mass-coordination function (which is real and valuable in its own right) and the deliberate extractive targeting (which is the specific historical claim this reading makes, evidenced by contemporary hostile testimony and printer account records).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation_press_reformation,
    'Is the printing press''s role in the Reformation best modeled as (a) an autonomous technology whose spread made Reformation success inevitable regardless of specific actors (technological_determinism), (b) a tool strategically selected and weaponized by specific agents with religious and economic goals (strategic_deployment, this story), or (c) a system co-constituted through feedback loops between print economy incentives and religious controversy content, where neither technology nor agency is separable as the prime mover (co_constitution)?',
    'Comparative archival analysis: strategic_deployment predicts documentary evidence of deliberate commissioning, timing, and targeting decisions in reformer/printer correspondence and account books; technological_determinism predicts print adoption patterns uncorrelated with individual strategic choices; co_constitution predicts observable feedback where commercial printer incentives shaped doctrinal content and vice versa, not reducible to either pole.',
    'Under strategic_deployment, the press is classified as an instrument (rope-like coordination substrate) deployed as a snare against the Curia by identifiable beneficiary agents — victims and beneficiaries are actors, not diffuse historical forces. Under technological_determinism, the constraint would classify closer to a mountain (no meaningful agent choice, inevitable diffusion) with no coherent victim/beneficiary structure. Under co_constitution, neither pure beneficiaries nor pure victims exist cleanly since the technology and the actors mutually shaped each other — that reading would likely classify as tangled_rope with diffuse rather than concentrated extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation_press_reformation, conceptual, 'The kernel-level disagreement between deterministic, strategic, and co-constitutive readings of the press''s causal role in the Reformation.').

omega_variable(
    printer_profit_motive_vs_ideological_commitment,
    'Were printers primarily profit-seeking commercial actors who would have printed Catholic material equally readily if the market rewarded it, or were they ideologically committed participants in the reformist project?',
    'Examination of printer output diversity — did major reformist print centers also produce orthodox Catholic material when commercially viable, or did ideological commitment override profit opportunities?',
    'If printers were pure profit-maximizers, the beneficiary classification of printer_guilds rests on market opportunism rather than shared strategic goals with reformers, which would slightly weaken the coordinated-weaponization reading in favor of a more diffuse commercial-exploitation reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_profit_motive_vs_ideological_commitment, empirical, 'Whether printer beneficiary status reflects ideological alignment or opportunistic commerce.').

omega_variable(
    counterfactual_curia_response_capacity,
    'Could the Roman Curia have deployed print as effectively as reformers did, had it chosen to prioritize the medium earlier, or was its structural position (centralized approval processes, Latin-first tradition, dispersed territorial authority) an inherent disadvantage regardless of strategic choice?',
    'Comparative study of the eventual Counter-Reformation Catholic print response (post-Trent) for evidence of whether effective Catholic print mobilization was achievable in principle and simply delayed, versus structurally foreclosed by institutional design.',
    'If the Curia''s disadvantage was purely a delayed strategic choice, the victim framing here is contingent on Rome''s own tactical failure, weakening the pure-extraction reading. If structurally foreclosed, the snare characterization of reformist deployment against an structurally defenseless target is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_curia_response_capacity, empirical, 'Whether the Curia''s vulnerability to print-based attack was contingent or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__strategic_deployment, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(pres_tr_t1522, press_reformation_causality__strategic_deployment, theater_ratio, 1522, 0.15).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causality__strategic_deployment, theater_ratio, 1530, 0.2).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__strategic_deployment, theater_ratio, 1540, 0.24).
narrative_ontology:measurement(pres_tr_t1548, press_reformation_causality__strategic_deployment, theater_ratio, 1548, 0.26).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__strategic_deployment, theater_ratio, 1555, 0.28).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(pres_be_t1522, press_reformation_causality__strategic_deployment, base_extractiveness, 1522, 0.48).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causality__strategic_deployment, base_extractiveness, 1530, 0.58).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__strategic_deployment, base_extractiveness, 1540, 0.64).
narrative_ontology:measurement(pres_be_t1548, press_reformation_causality__strategic_deployment, base_extractiveness, 1548, 0.66).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__strategic_deployment, base_extractiveness, 1555, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(pres_su_t1522, press_reformation_causality__strategic_deployment, suppression_requirement, 1522, 0.42).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causality__strategic_deployment, suppression_requirement, 1530, 0.52).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__strategic_deployment, suppression_requirement, 1540, 0.58).
narrative_ontology:measurement(pres_su_t1548, press_reformation_causality__strategic_deployment, suppression_requirement, 1548, 0.6).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__strategic_deployment, suppression_requirement, 1555, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causality kernel. technological_determinism authors the press as an autonomous mountain-like force; co_constitution authors it as a tangled_rope-like feedback system with diffuse, non-agent-specific extraction; strategic_deployment (this story) authors it as a rope-substrate weaponized into a snare by specific, named, strategically-motivated agents. All three share the same historical events but differ in where they locate causal agency and therefore in beneficiary/victim structure and classification. Linked via affects_constraints in all three files per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
