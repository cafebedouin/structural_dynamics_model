% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation — International Regime Deferral Reading
 *   domain: international law / commons governance
 *
 * SUMMARY:
 *   Outer Space Treaty Article II bars national appropriation of celestial
 *   territory 'by claim of sovereignty, by means of use or occupation, or by
 *   any other means.' What that text does for RESOURCE extraction is the
 *   contested kernel ost_article_ii_non_appropriation, and this file
 *   instantiates exactly one of its three readings: the international_regime
 *   reading, under which Article II deliberately left the
 *   appropriation-of-resources question open, deferring it to a future
 *   multilateral framework — the structure later codified for the Moon in
 *   Moon Agreement Article XI. On this reading the operative arrangement is a
 *   transitional holding pattern: no extraction license and no prohibition
 *   carries treaty authority; national statutes and bloc accords fill the
 *   silence provisionally; and the arrangement's justification is the
 *   transition to a negotiated regime, not the steady state. The eps referent
 *   is the standing arrangement under contest — the current grey zone itself,
 *   assessed by this reading's lights — never the regime this reading
 *   endorses. Sibling readings (extraction_permissive, commons_conservation)
 *   are separate constraint files linked via network.affects_constraints;
 *   their identifiers follow the kernel-prefixed naming convention assumed
 *   here. The claim/metric gap is deliberate: the reading CLAIMS a
 *   transitional scaffold while the authored metrics describe rising
 *   first-mover capture and rising declaratory substitution for negotiation —
 *   the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - - first_mover_extraction_firms: Primary beneficiary (powerful/arbitrage) — converts legal uncertainty into physical facts and market position
 *   - - national_licensing_states: Dual-positioned beneficiary/agenda-setter (institutional/mobile) — fills the treaty silence with national law that advantages their own operators
 *   - - non_spacefaring_states: Primary payer (organized/trapped) — deferred common-pool claim erodes yearly with no capacity to extract or exit
 *   - - latecomer_commercial_operators: Payer (moderate/constrained) — enters through licensing systems designed by incumbents' home states
 *   - - planetary_science_community: Payer (moderate/constrained) — unique sites consumed before characterization
 *   - - un_copuos_secretariat: Agenda-setter without decision power (institutional/constrained)
 *   - - moon_agreement_states_parties: Excluded voice (organized/trapped) — completed regime framework sidelined for lack of major-power ratification
 *   - - space_law_scholars: Analytical observer (analytical/analytical) — sees the full three-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.46).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.24).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.46).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation — International Regime Deferral Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international law / commons governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '2f4c3d15-c428-4902-9d59-a8efa995deff').
narrative_ontology:cs_kernel_codification('2f4c3d15-c428-4902-9d59-a8efa995deff', fixed_text).
narrative_ontology:cs_authority_grounding('2f4c3d15-c428-4902-9d59-a8efa995deff', distributed).
narrative_ontology:cs_reading_relation('2f4c3d15-c428-4902-9d59-a8efa995deff', ost_article_ii_non_appropriation__ost_article_ii_extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('2f4c3d15-c428-4902-9d59-a8efa995deff', ost_article_ii_non_appropriation__ost_article_ii_commons_conservation, coexists_with).
narrative_ontology:cs_axiom('2f4c3d15-c428-4902-9d59-a8efa995deff', foundational, resource_appropriation_deferred_to_international_regime).
narrative_ontology:cs_axiom_status(resource_appropriation_deferred_to_international_regime, holdable).
narrative_ontology:cs_axiom_grounding('2f4c3d15-c428-4902-9d59-a8efa995deff', resource_appropriation_deferred_to_international_regime, conventional).
narrative_ontology:cs_axiom('2f4c3d15-c428-4902-9d59-a8efa995deff', secondary, unilateral_national_licensing_lacks_binding_force).
narrative_ontology:cs_axiom_status(unilateral_national_licensing_lacks_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('2f4c3d15-c428-4902-9d59-a8efa995deff', unilateral_national_licensing_lacks_binding_force, conventional).
narrative_ontology:cs_reference_frame('2f4c3d15-c428-4902-9d59-a8efa995deff', provisional_deferral_pending_multilateral_regime).
narrative_ontology:cs_drift_state('2f4c3d15-c428-4902-9d59-a8efa995deff', commercial_capability_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f4c3d15-c428-4902-9d59-a8efa995deff', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, national_licensing_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, latecomer_commercial_operators).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, planetary_science_community).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, deferred_allocation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and fly the demonstration missions that turn lunar regolith and asteroid material into saleable commodities. While no treaty rule settles who owns extracted material, they take authorization from whichever national government will license them, book the resulting property rights under that national law, and owe no international fee, royalty, or benefit-sharing obligation. Title risk hangs over their books — a future multilateral framework could reprice or invalidate their claims — but every year of operation converts legal uncertainty into physical facts and market position. Leaving the field means surrendering first-mover position to rivals.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Enact domestic statutes authorizing their nationals to extract and own space resources, filling the treaty silence with national law. Each statute strengthens their firms' competitive position and their own standing as the de facto governing authority, while stopping short of sovereignty claims that would openly contradict Article II. They can shift between the multilateral negotiating track and unilateral consolidation depending on which pays better in a given year.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, national_licensing_states, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, national_licensing_states, agenda_setter).

% Hold treaty-based expectations that celestial resources belong to a common pool whose benefits will eventually be shared, but possess no launch capability and no licensed operators. Every year the question stays open, first movers convert the common pool into privately held stockpile under national titles, eroding the value of the claim they agreed to defer. They act through the G77 and COPUOS consensus procedures, where they can delay but not decide. Exiting the treaty order is not available to them; its rules bind them regardless of their conduct.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states, payer,
    organized, generational, trapped, global).

% Enter the field after incumbents have secured licenses, launch contracts, and customer relationships under favorable national regimes. They must route their ambitions through licensing systems designed by the incumbents' home governments, carry the same title uncertainty at greater competitive disadvantage, and cannot relocate to a more favorable jurisdiction that does not yet exist.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, latecomer_commercial_operators, payer,
    moderate, biographical, constrained, global).

% Study pristine lunar and asteroid environments whose scientific value depends on remaining undisturbed. Unlicensed extraction activity threatens unique sites — polar ice deposits, stratigraphically significant craters — before they are characterized. They hold advisory standing in COPUOS but no vote over licensing decisions made in national capitals.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, planetary_science_community, payer,
    moderate, generational, constrained, global).

% Convenes the Working Group on Legal Aspects of Space Resource Activities, circulates discussion papers, and brokers draft text. It can place questions on the agenda and build consensus packages but cannot adopt binding rules; its output is capped at what rival blocs will jointly sign, which so far is guidelines rather than allocation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, un_copuos_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Ratified the 1979 agreement declaring lunar resources the common heritage of humankind and directing creation of an international regime — the framework this deferral promises. The major space powers never joined, leaving their completed instrument sidelined. They continue pressing their reading in COPUOS with diminishing leverage and no procedural path to compel ratification.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, moon_agreement_states_parties, excluded,
    organized, generational, trapped, global).

% Analyze the treaty text, the 1966-67 drafting history, and accumulating state practice; publish the competing interpretations; advise delegations on all sides. They see the full three-reading structure of the dispute and hold no stake in which reading prevails.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds the celestial-resource allocation question open until a multilateral framework can decide it: by leaving Article II's application to resource extraction undetermined, the arrangement prevents any single bloc's reading from hardening into custom while capabilities and stakes are still forming, and keeps all parties' claims negotiable.
% TRANSFER_FUNCTION: Moves de facto control of celestial material from the common pool to whichever operators secure national licenses first, and moves regulatory authority from the prospective multilateral forum to national governments willing to legislate; the deferred community share accrues to no one during the window.
% ABSENT_VOICES: Future generations and the non-spacefaring majority hold formal seats in COPUOS but no agenda power; the Moon Agreement parties' completed framework sits sidelined without major-power ratification; smaller commercial entrants are represented only through the licensing states that host them. Working Group consensus reflects the seats present, not the parties affected.
% DISAPPEARANCE_RATIONALE: If the deferral ended overnight — by binding ruling or sudden regime adoption — extraction would either proceed under recognized national titles or halt pending allocation; licensed operations, insurance markets, and investor positions built on the grey zone would reprice immediately, and the COPUOS negotiating track would either dissolve or rush to completion. The parties' arrangements visibly depend on the current state of the question.
% FOUNDING_PROBLEM: The 1967 drafters faced Cold War extension into orbit and onto the Moon: sovereign flags, military bases, territorial annexation. Article II answered the sovereignty question categorically while leaving the then-unforeseeable question of private resource appropriation expressly untouched — on this reading, a deliberate deferral to a future framework equipped to allocate.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the published drafting record of the Outer Space Treaty negotiations (1966-67, preserved in UN documents and scholarly compilations) corroborates that resource appropriation was consciously left unresolved; the UN General Assembly's 1979 adoption of the Moon Agreement's Article XI regime mechanism attests that the community of states understood a regime question to be open; and the COPUOS Working Group convenes on the shared premise that the question remains unsettled. Extraction-permissive states dispute the deferral characterization itself, which is what keeps the status contested rather than dead.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.46 at interval end) is moderate and rising: the deferral itself seizes nothing, but the grey zone it constitutes enables uncompensated first-mover conversion of a commons into private stockpile under national title, and every year of maturing capability raises the transfer rate. Suppression (0.24) is low because the arrangement is maintained by diplomatic stalemate rather than coercion — no machinery punishes deviation; the modest suppression_requirement series models the growing diplomatic effort needed to KEEP the question open (blocking consensus, sustaining parallel fora) as capabilities mature, which is the story's enforcement-capacity dynamic. Theater ratio (0.55) exceeds half: guidelines, building-block documents, and accord-signing ceremonies increasingly substitute declaratory activity for allocation decisions while the Working Group produces non-binding text. Accessibility_collapse (0.32) is low — alternatives (national licensing statutes, bilateral and bloc accords) remain fully available and heavily used; nothing closes them off. Resistance (0.58) is substantial and bidirectional: the permissive camp races to lock in national law before a regime forms, while the conservation camp pushes moratorium-style readings; both resist the deferral itself. All three series run on one shared time grid (2015/2018/2021/2024/2027/2030) so no metric row borrows another's end-state; 2027 and 2030 points are authored projections and marked as such.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently from identical nominal facts. From the first mover's position the grey zone is operating room: arbitrage-grade exit (forum-shopping among licensing states) places it near the beneficiary end of directionality, so effective extraction is damped. From the non-spacefaring states' position the same arrangement is a slow expropriation of a deferred claim: trapped exit amplifies their effective extraction toward the full-target end. The licensing states sit between — collectors of competitive advantage and administrators of the vacuum at once. The engine computes these per-seat divergences from the structural data; the authored scaffold claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for first_mover_extraction_firms (subsidized by the uncertainty, arbitrage exit pushes further toward the beneficiary pole) and national_licensing_states (collect competitive advantage; their secondary agenda-setting role moderates but does not reverse the derivation). Victim declarations drive high d for non_spacefaring_states (trapped, so near-full target), latecomer_commercial_operators (constrained entry), and planetary_science_community (constrained, diffuse but real losses). No directionality overrides were needed: the beneficiary/victim plus exit-option data produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is what prevents mislabeling in both directions. Read as pure coordination, the deferral looks like a rope — everyone's claims preserved, nobody coerced — which conceals the asymmetric transfer running through it toward first movers. Read as pure obstruction, it looks like a snare blocking development, which conceals that its justification is transitional and that its principal payers include actors who want the transition to complete. The scaffold form holds both truths: genuine transitional function, declared sunset, and a measurable extraction leakage that grows as the window stays open. The mandatrophy risk is decay rather than resolution: if the regime never forms, the deferral outlives its function and degrades into an inertial arrangement maintained by ritual Working Group sessions — the sunset_trigger_timing and negotiation_stall omegas are the tracking instruments for that branch. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges, which raises no dead-arrangement flag; the founding question is disputed, not abandoned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authoritative_reading_unresolved,
    'This constraint is one of three live readings of the kernel ost_article_ii_non_appropriation (readings: extraction_permissive, commons_conservation, international_regime). Which reading will attain authoritative status?',
    'Binding adjudication (ICJ advisory opinion), adoption of a multilateral space-resource regime, or convergent state practice crystallizing one reading as customary law.',
    'If the extraction-permissive sibling attains authority, this deferral arrangement terminates into a permissive steady state with its own epsilon and victim set; if the commons-conservation sibling wins, it terminates into a prohibition regime. This file instantiates only the deferral branch and hedges neither.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authoritative_reading_unresolved, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    sunset_trigger_timing,
    'What event actually terminates the deferral — formal adoption of a multilateral regime, or de facto hardening of national allocation past the point where a regime could override it?',
    'Track COPUOS Working Group output against the accumulation of licensed extraction operations; the Moon Agreement''s own trigger (''as the exploitation of the natural resources of the moon is about to become feasible'') supplies the doctrinal test.',
    'If de facto hardening outruns negotiation, the deferral decays from a transitional arrangement into an inertial one maintained theatrically; if negotiation lands first, the sunset executes as designed and the transitional justification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_trigger_timing, empirical, 'Whether the declared sunset executes or the arrangement outlives its transition.').

omega_variable(
    fait_accompli_grandfathering,
    'Will resources converted to private stockpiles and national-title operations during the deferral window be grandfathered into any future regime, or repriced on entry?',
    'Transitional provisions in the eventual regime instrument; negotiating positions on acquired rights; precedent from comparable commons settlements (deep-seabed Part XI compromise, fisheries quota grandfathering).',
    'If grandfathered, the deferral functions as a one-way transfer to first movers despite its formally neutral form, and the measured extractiveness understates the realized transfer; if repriced, the deferral''s costs are recoverable and its transitional character is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fait_accompli_grandfathering, empirical, 'Reversibility of the facts created inside the deferral window.').

omega_variable(
    negotiation_stall_structural_or_circumstantial,
    'Is the regime-negotiation stall a bargaining impasse that changed circumstances (capability shocks, a salvage or collision incident, a resource-price crisis) could break, or a structural feature of zero-sum distributional conflict among the licensing blocs?',
    'Comparative analysis of stalled commons negotiations that later closed (1994 deep-seabed implementation agreement) against those that never did; identification of the specific concession each bloc requires and whether any coalition can deliver it.',
    'If circumstantial, the arrangement retains a live sunset and its transitional justification stands; if structural, the deferral persists indefinitely without executing its purpose and the decay branch of sunset_trigger_timing dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_stall_structural_or_circumstantial, preference, 'Whether the zero-sum stall is contingent or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(ost__tr_t2015, observed).
narrative_ontology:measurement(ost__tr_t2018, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2018, 0.44).
narrative_ontology:measurement_basis(ost__tr_t2018, observed).
narrative_ontology:measurement(ost__tr_t2021, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2021, 0.47).
narrative_ontology:measurement_basis(ost__tr_t2021, observed).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2024, 0.5).
narrative_ontology:measurement_basis(ost__tr_t2024, observed).
narrative_ontology:measurement(ost__tr_t2027, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2027, 0.53).
narrative_ontology:measurement_basis(ost__tr_t2027, projected).
narrative_ontology:measurement(ost__tr_t2030, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2030, 0.55).
narrative_ontology:measurement_basis(ost__tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement_basis(ost__be_t2015, observed).
narrative_ontology:measurement(ost__be_t2018, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2018, 0.27).
narrative_ontology:measurement_basis(ost__be_t2018, observed).
narrative_ontology:measurement(ost__be_t2021, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2021, 0.33).
narrative_ontology:measurement_basis(ost__be_t2021, observed).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2024, 0.39).
narrative_ontology:measurement_basis(ost__be_t2024, observed).
narrative_ontology:measurement(ost__be_t2027, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2027, 0.43).
narrative_ontology:measurement_basis(ost__be_t2027, projected).
narrative_ontology:measurement(ost__be_t2030, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2030, 0.46).
narrative_ontology:measurement_basis(ost__be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement_basis(ost__su_t2015, observed).
narrative_ontology:measurement(ost__su_t2018, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2018, 0.17).
narrative_ontology:measurement_basis(ost__su_t2018, observed).
narrative_ontology:measurement(ost__su_t2021, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2021, 0.2).
narrative_ontology:measurement_basis(ost__su_t2021, observed).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2024, 0.22).
narrative_ontology:measurement_basis(ost__su_t2024, observed).
narrative_ontology:measurement(ost__su_t2027, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2027, 0.23).
narrative_ontology:measurement_basis(ost__su_t2027, projected).
narrative_ontology:measurement(ost__su_t2030, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2030, 0.24).
narrative_ontology:measurement_basis(ost__su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_commons_conservation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what Article II means for space resource appropriation' decomposes into three structurally distinct claims per the epsilon-invariance principle — measuring the text as a permission statement, a prohibition statement, and a deferral statement yields materially different epsilon values, victim sets, and classifications, so they are three files, not one story with a measurement parameter. This deferral reading is structurally downstream of the textual indeterminacy the other two exploit: each sibling's plausibility depends on the deferral persisting long enough for its proponents to consolidate facts. Family members link mutually via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
