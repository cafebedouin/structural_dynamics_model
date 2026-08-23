% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Printing Press as Strategic Tool of Reformation Actors
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story represents the 'strategic deployment' reading of
 *   the press_reformation_causation kernel. It asserts that the printing
 *   press was a neutral technological capacity that Reformation actors
 *   (reformers, printers, city councils) deliberately exploited as a
 *   coordination tool. The press functioned as a ROPE: it solved a genuine
 *   collective-action problem (mass dissemination across fragmented polities)
 *   with minimal coercive overhead; participants (reformers, printers,
 *   cities) were net beneficiaries; alternatives (manuscript networks, oral
 *   preaching) were not suppressed by the press itself — they atrophied
 *   because the press was more effective. The claim/metric independence is
 *   maintained: claimed_type = rope (low extraction, low suppression), while
 *   metrics reflect the reading's assessment of the historical constraint's
 *   actual operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.25).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.15).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.25).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Printing Press as Strategic Tool of Reformation Actors").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, 'c000944e-6703-4f12-b7e6-e6cc04e12521').
narrative_ontology:cs_kernel_codification('c000944e-6703-4f12-b7e6-e6cc04e12521', distributed).
narrative_ontology:cs_authority_grounding('c000944e-6703-4f12-b7e6-e6cc04e12521', practice).
narrative_ontology:cs_reading_relation('c000944e-6703-4f12-b7e6-e6cc04e12521', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('c000944e-6703-4f12-b7e6-e6cc04e12521', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('c000944e-6703-4f12-b7e6-e6cc04e12521', foundational, technology_is_neutral_capacity).
narrative_ontology:cs_axiom_status(technology_is_neutral_capacity, holdable).
narrative_ontology:cs_axiom_grounding('c000944e-6703-4f12-b7e6-e6cc04e12521', technology_is_neutral_capacity, conventional).
narrative_ontology:cs_axiom('c000944e-6703-4f12-b7e6-e6cc04e12521', foundational, agency_is_upstream_driver_of_media_effects).
narrative_ontology:cs_axiom_status(agency_is_upstream_driver_of_media_effects, holdable).
narrative_ontology:cs_axiom_grounding('c000944e-6703-4f12-b7e6-e6cc04e12521', agency_is_upstream_driver_of_media_effects, conventional).
narrative_ontology:cs_reference_frame('c000944e-6703-4f12-b7e6-e6cc04e12521', pre_print_manuscript_gatekeeping).
narrative_ontology:cs_drift_state('c000944e-6703-4f12-b7e6-e6cc04e12521', post_reformation_confessionalization, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('c000944e-6703-4f12-b7e6-e6cc04e12521', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformation_printers_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, vernacular_literacy_advocates).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_institutional_authority).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, traditional_manuscript_scribes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, imperial_free_cities_councils).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, peasant_and_urban_populations).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, peasant_and_urban_populations).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, technology_as_neutral_capacity).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, agency_as_upstream_driver).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, strategic_deployment_of_media).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Luther, Calvin, Zwingli and their networks deliberately used the press to disseminate vernacular scripture, polemics, and organizational materials. They negotiated with printers, oversaw translations, and treated the technology as a strategic instrument for doctrinal propagation and institutional building. Their exit options included manuscript networks and oral preaching, but the press offered unprecedented scale and speed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, protestant_reformers, beneficiary).

% Printers in Wittenberg, Basel, Strasbourg, Geneva, and other centers profited enormously from Reformation demand — pamphlets, vernacular Bibles, catechisms, hymnals. They invested in typefaces, distribution networks, and editorial expertise. Their exit was constrained by capital investment in presses and type, and by reliance on Reformation markets; but they were not trapped — many printed for Catholic clients too when profitable.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, reformation_printers_publishers, beneficiary,
    moderate, biographical, constrained, regional).

% Humanists, educators, and civic leaders who saw vernacular print as a vehicle for lay literacy, civic participation, and cultural standardization. They benefited from the infrastructure the Reformation built but their aims were not exclusively theological. They could pursue their goals through manuscript culture or state schooling, though less efficiently.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, vernacular_literacy_advocates, beneficiary,
    moderate, generational, mobile, continental).

% The papacy, episcopal courts, Inquisition, and Catholic princes lost control over doctrinal dissemination and faced a proliferation of unauthorized texts. They responded with censorship (Index Librorum Prohibitorum), press licensing, and polemical printing of their own. Their exit from the constraint was constrained by the technology's diffusion — they could not un-invent the press, only attempt to regulate it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_institutional_authority, payer,
    institutional, civilizational, constrained, continental).

% Scriptoria and professional scribes saw their livelihood collapse as printed books undercut manuscript production. They had no structural power to resist, no alternative employment at comparable status, and no exit from the technological displacement. A few transitioned to proofreading or corrector roles in print shops, but most were displaced.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, traditional_manuscript_scribes, payer,
    powerless, biographical, trapped, local).

% City councils in Strasbourg, Nuremberg, Basel, Zurich, and elsewhere used press regulation to attract Reformation printers, gain fiscal revenue, and assert autonomy from both emperor and pope. They granted printing privileges, regulated content, and benefited from the prestige and economy of print centers. They could choose to suppress or enable printing based on political calculation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, imperial_free_cities_councils, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, imperial_free_cities_councils, beneficiary).

% Ordinary people gained access to vernacular scripture, hymns, and religious instruction in their own language — a genuine coordination benefit. But they also bore the costs of religious war, social disruption, and confessional coercion. They had no meaningful exit from the territories where they lived; their 'consent' to the new media environment was not structurally available.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, peasant_and_urban_populations, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, peasant_and_urban_populations, payer).

% Catholic printers and controversialists (e.g., Eck, Cochlaeus) were not structurally excluded from the technology — they used it extensively. But they were excluded from the *strategic initiative*: they reacted to Reformation printing rather than setting the agenda. Their constraint was playing defense with the same tool.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, competing_printers_catholic_polemists, excluded,
    moderate, biographical, constrained, continental).

% Scholars who analyze the press-Reformation relationship from outside the historical moment. They inherit the categorical dispute (determinism vs. agency vs. mutual shaping) and their readings shape contemporary understanding of media revolutions. They do not collect rents or bear costs from the historical constraint.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, modern_media_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solved a genuine collective-action problem for Reformation actors: how to disseminate a coherent doctrinal and organizational program across linguistic and political boundaries at speed and scale that manuscript networks could not achieve. It standardized texts, enabled rapid iteration of polemics, and created a shared reference field for distributed reform movements.
% TRANSFER_FUNCTION: The arrangement moved three things: (1) Capital — from book-buying publics and patron cities to printers and paper-makers; (2) Doctrinal authority — from clerical monopoly to lay-accessible vernacular text; (3) Political legitimacy — from imperial/universal structures to territorial princes and city councils who could authorize or suppress print. The transfer was not zero-sum: printers profited, reformers gained reach, cities gained revenue and autonomy, but Catholic institutional authority lost control.
% ABSENT_VOICES: Women printers and distributors (e.g., Katharina von Bora's role in Luther's household press operations, women running print shops in Basel and Lyon) are under-documented in the surviving record. Jewish printers in Venice and Constantinople who produced Hebrew texts and polyglot Bibles operated in parallel but are rarely integrated into the Reformation press narrative. Peasant rebels of 1524-25 who used printed articles to coordinate demands had no seat at the tables where press privileges were granted.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the press by reformers and printers vanished overnight (counterfactual: Gutenberg's invention occurs but no actor treats it as a tool for doctrinal propaganda), the Reformation as a mass movement fails — Luther's theses remain a local academic dispute, vernacular Bibles do not saturate German lands, and the Catholic Church retains doctrinal gatekeeping. The world rearranges: no confessional division of Europe, no print-driven standardization of vernaculars, no 'public sphere' precursor.
% FOUNDING_PROBLEM: The founding problem was the Catholic Church's effective monopoly on doctrinal dissemination and textual authority, enforced through manuscript control, Latin literacy requirements, and episcopal censorship. Reformers and printers needed a way to bypass these gatekeepers at scale.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead: the Catholic Church no longer controls textual dissemination, Latin is not the gatekeeper language, and manuscript monopoly is gone. This is corroborated by Catholic historians (e.g., Hubert Jedin, John O'Malley) who acknowledge the Church's loss of media control as a structural fact, not a Protestant polemic. The arrangement (press as strategic tool) persists in modern form — political actors still strategically deploy new media — but the *specific* founding problem is resolved.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).
:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the press primarily enabled coordination — the gains (doctrinal reach, printer profit, civic autonomy) were not extracted from a captive population but produced by solving a dissemination problem. Suppression is low (0.15) because the press itself did not coerce; Catholic authorities attempted suppression *against* the press, not through it. Theater ratio is very low (0.10) — the coordination function was genuine and the performative layer (e.g., printer's devices, prefaces claiming authority) was thin relative to functional output. Accessibility collapse is moderate (0.65) — manuscript culture did not vanish but became marginal for mass dissemination. Resistance is high (0.75) — the Catholic Church mounted vigorous institutional resistance (Index, Inquisition, licensing), confirming the constraint threatened established power.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer/printer seat, the press is a rope — a tool they chose and shaped. From the Catholic authority seat, the same technology appears as a snare — an uncontrolled force eroding their legitimacy. From the scribe seat, it is a mountain — an irreversible technological shift that destroyed their livelihood. The engine computes these divergences from the declared power/exit/role data; the authored claim (rope) reflects the *primary* coordination function from the strategic actors' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are structural beneficiaries (d near 0.0) — they gained reach, revenue, and institutional capacity. Catholic authority is a structural payer (d near 1.0) — it lost gatekeeping control and faced competitive dissemination. Scribes are trapped payers (d = 1.0, exit_options: trapped) — displaced with no alternatives. Urban populations are dual-role: beneficiaries of vernacular access but payers of social disruption costs (d ~ 0.5). City councils are agenda_setters with mobile exit — they could choose to host or suppress presses. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Catholic textual monopoly) is dead, but the strategic deployment pattern persists: every new medium (radio, TV, internet, social media) is strategically exploited by political/religious actors. The original constraint (Reformation press deployment) resolved its mandatrophy by transforming into a generalized pattern — the 'press as strategic tool' reading remains live as an analytical lens, not as a persistent historical arrangement. The mandate (bypass gatekeepers) was fulfilled; the tool remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the strategic_deployment reading a genuinely distinct causal claim from mutual_shaping, or does it merely emphasize a different pole of the same co-evolutionary process?',
    'Counterfactual test: if reformers had not strategically adopted the press (e.g., Luther refuses print, relies on sermons), would the Reformation still have occurred at comparable scale and speed via manuscript/oral networks? If no, strategic_deployment is causally distinct; if yes, it is an emphasis within mutual_shaping.',
    'If distinct, this reading warrants its own constraint story with its own ε (0.25 here). If not distinct, it should merge with mutual_shaping as a perspectival variant, not a separate constraint. This affects whether the kernel has 2 or 3 genuine constraint children.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether strategic_deployment and mutual_shaping are structurally distinct constraints or perspectival variants of one constraint.').

omega_variable(
    press_neutrality_claim,
    'Was the press truly ''neutral capacity'' or did its material affordances (fixed type, reproducibility, economics of scale) structurally favor certain messages and actors over others?',
    'Compare diffusion patterns: did Catholic controversialists achieve equivalent reach per unit investment? Did the economics of print (high fixed cost, low marginal cost) inherently favor high-volume polemic over nuanced scholarly exchange? Analyze print runs, pricing, and distribution networks for Catholic vs. Protestant output 1517-1550.',
    'If the press''s material affordances structurally favored Reformation-style dissemination (short, vernacular, high-volume), then ''neutral capacity'' is false — the technology had a built-in bias that the strategic_deployment reading must acknowledge. This would increase extractiveness (the press extracts conformity to its affordances) and shift claimed_type toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(press_neutrality_claim, empirical, 'Whether the printing press was a neutral tool or had affordances that structurally favored Reformation messaging.').

omega_variable(
    printer_agency_vs_market_pressure,
    'Did printers *choose* Reformation work for profit/ideology, or were they compelled by market demand (Reformation texts sold; Catholic texts didn''t)?',
    'Examine printer correspondence, financial records, and output mixes in mixed-confession cities (Augsburg, Nuremberg, Strasbourg). Did printers print both sides? Did they switch confessional output based on local politics? Were there printers who refused Reformation work despite demand?',
    'If printers were market-compelled, their ''beneficiary'' status is partial — they captured gains but had low agency. If they were strategic choosers, they are genuine agenda_setters/beneficiaries. This affects directionality for the printer stakeholder seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_agency_vs_market_pressure, empirical, 'Whether printer participation was strategic agency or market compulsion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reformation_strategic_tr_t1450, press_reformation_causation__strategic_deployment, theater_ratio, 1450, 0.05).
narrative_ontology:measurement_basis(press_reformation_strategic_tr_t1450, observed).
narrative_ontology:measurement(press_reformation_strategic_tr_t1480, press_reformation_causation__strategic_deployment, theater_ratio, 1480, 0.08).
narrative_ontology:measurement_basis(press_reformation_strategic_tr_t1480, observed).
narrative_ontology:measurement(press_reformation_strategic_tr_t1517, press_reformation_causation__strategic_deployment, theater_ratio, 1517, 0.1).
narrative_ontology:measurement_basis(press_reformation_strategic_tr_t1517, observed).
narrative_ontology:measurement(press_reformation_strategic_tr_t1530, press_reformation_causation__strategic_deployment, theater_ratio, 1530, 0.1).
narrative_ontology:measurement_basis(press_reformation_strategic_tr_t1530, observed).
narrative_ontology:measurement(press_reformation_strategic_tr_t1550, press_reformation_causation__strategic_deployment, theater_ratio, 1550, 0.1).
narrative_ontology:measurement_basis(press_reformation_strategic_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(press_reformation_strategic_be_t1450, press_reformation_causation__strategic_deployment, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement_basis(press_reformation_strategic_be_t1450, observed).
narrative_ontology:measurement(press_reformation_strategic_be_t1480, press_reformation_causation__strategic_deployment, base_extractiveness, 1480, 0.2).
narrative_ontology:measurement_basis(press_reformation_strategic_be_t1480, observed).
narrative_ontology:measurement(press_reformation_strategic_be_t1517, press_reformation_causation__strategic_deployment, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement_basis(press_reformation_strategic_be_t1517, observed).
narrative_ontology:measurement(press_reformation_strategic_be_t1530, press_reformation_causation__strategic_deployment, base_extractiveness, 1530, 0.25).
narrative_ontology:measurement_basis(press_reformation_strategic_be_t1530, observed).
narrative_ontology:measurement(press_reformation_strategic_be_t1550, press_reformation_causation__strategic_deployment, base_extractiveness, 1550, 0.25).
narrative_ontology:measurement_basis(press_reformation_strategic_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(press_reformation_strategic_su_t1450, press_reformation_causation__strategic_deployment, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement_basis(press_reformation_strategic_su_t1450, observed).
narrative_ontology:measurement(press_reformation_strategic_su_t1480, press_reformation_causation__strategic_deployment, suppression_requirement, 1480, 0.12).
narrative_ontology:measurement_basis(press_reformation_strategic_su_t1480, observed).
narrative_ontology:measurement(press_reformation_strategic_su_t1517, press_reformation_causation__strategic_deployment, suppression_requirement, 1517, 0.15).
narrative_ontology:measurement_basis(press_reformation_strategic_su_t1517, observed).
narrative_ontology:measurement(press_reformation_strategic_su_t1530, press_reformation_causation__strategic_deployment, suppression_requirement, 1530, 0.15).
narrative_ontology:measurement_basis(press_reformation_strategic_su_t1530, observed).
narrative_ontology:measurement(press_reformation_strategic_su_t1550, press_reformation_causation__strategic_deployment, suppression_requirement, 1550, 0.15).
narrative_ontology:measurement_basis(press_reformation_strategic_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__strategic_deployment, 0.02).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, vernacular_standardization_via_print).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, public_sphere_emergence_early_modern).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the 'press_reformation_causation' kernel. The technological_determinism reading claims the press *caused* the Reformation (high extractiveness from Catholic view, mountain-like inevitability). The mutual_shaping reading claims co-evolution (moderate extractiveness, tangled_rope). This strategic_deployment reading claims agency-driven coordination (low extractiveness, rope). All three share the same historical referent but instantiate different constraints with different ε, stakeholders, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causation__strategic_deployment, organized, 0.15).
constraint_indexing:directionality_override(press_reformation_causation__strategic_deployment, moderate, 0.3).
constraint_indexing:directionality_override(press_reformation_causation__strategic_deployment, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
