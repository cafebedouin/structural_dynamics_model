% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: OST Article II Extraction-Permissive Reading: Sovereignty Bar Without Resource-Title Bar
 *   domain: international law/commons governance/treaty interpretation
 *
 * SUMMARY:
 *   Article II of the 1967 Outer Space Treaty declares the Moon and other
 *   celestial bodies 'not subject to national appropriation.' The
 *   extraction-permissive reading - instantiated here and embedded in the
 *   2015 US Commercial Space Launch Competitiveness Act, the 2017 Luxembourg
 *   law, the 2019 UAE and Japanese statutes, and the Artemis Accords - holds
 *   that this bars sovereign territorial claims only, leaving private
 *   ownership of recovered resources lawful under flag-state title grants.
 *   The standing arrangement under contest (and the epsilon referent for this
 *   story) is that arrangement itself: unilateral national licensing of
 *   resource recovery, access gated by technological capability and
 *   flag-state recognition, no compensation mechanism for excluded states,
 *   enclosure accumulating by accomplished fact rather than formal
 *   annexation. This story is one member of a three-story constraint family
 *   decomposing the Article II kernel per the epsilon-invariance principle:
 *   the commons_conservation sibling authors the same text as barring de
 *   facto appropriation through recovery (covering private actors), and the
 *   international_regime sibling authors the deferral of the whole question
 *   to a future multilateral framework. Each sibling gets its own epsilon,
 *   its own victim set, and its own classification; they are linked, not
 *   merged. The claim and the metrics are independent authored facts:
 *   claimed_type tangled_rope reflects the structure (a genuine
 *   sovereignty-bar coordination function carrying an enforced
 *   asymmetric-access overlay), while the metrics describe the arrangement's
 *   actual operation as the structural delta specifies - substantially
 *   extractive, actively maintained, moderately theatrical.
 *
 * KEY AGENTS:
 *   - - spacefaring_flag_states: Agenda-setter (institutional/arbitrage) - authors title statutes, registers operators, defends the reading diplomatically
 *   - - private_extraction_operators: Primary beneficiary (organized/mobile) - receives vested title to recovered material under flag-state law
 *   - - artemis_accords_signatories: Secondary beneficiary with payer exposure (moderate/constrained) - buys into the order on drafter-written terms
 *   - - non_spacefaring_developing_states: Primary target (organized/trapped) - bears uncompensated exclusion from resource streams
 *   - - emerging_space_nations: Late-entrant target with prospective beneficiary position (moderate/constrained)
 *   - - space_resource_commons_advocates: Excluded voice (moderate/identity_locked) - argues the rival reading from outside any decision seat
 *   - - copuos_legal_committee: Analytical observer (institutional/analytical) - hosts the contest, decides nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.72).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.7).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.72).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "OST Article II Extraction-Permissive Reading: Sovereignty Bar Without Resource-Title Bar").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international law/commons governance/treaty interpretation").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '21d53190-6635-4661-bf48-025ce5afeef9').
narrative_ontology:cs_kernel_codification('21d53190-6635-4661-bf48-025ce5afeef9', fixed_text).
narrative_ontology:cs_authority_grounding('21d53190-6635-4661-bf48-025ce5afeef9', lineage).
narrative_ontology:cs_interpretation_layer_present('21d53190-6635-4661-bf48-025ce5afeef9').
narrative_ontology:cs_reading_relation('21d53190-6635-4661-bf48-025ce5afeef9', ost_article_ii_non_appropriation__ost_article_ii_commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('21d53190-6635-4661-bf48-025ce5afeef9', ost_article_ii_non_appropriation__ost_article_ii_international_regime, influences).
narrative_ontology:cs_axiom('21d53190-6635-4661-bf48-025ce5afeef9', foundational, extracted_material_private_ownable).
narrative_ontology:cs_axiom_status(extracted_material_private_ownable, holdable).
narrative_ontology:cs_axiom_grounding('21d53190-6635-4661-bf48-025ce5afeef9', extracted_material_private_ownable, conventional).
narrative_ontology:cs_axiom('21d53190-6635-4661-bf48-025ce5afeef9', foundational, capability_gated_access_without_compensation).
narrative_ontology:cs_axiom_status(capability_gated_access_without_compensation, holdable).
narrative_ontology:cs_axiom_grounding('21d53190-6635-4661-bf48-025ce5afeef9', capability_gated_access_without_compensation, instrumental).
narrative_ontology:cs_reference_frame('21d53190-6635-4661-bf48-025ce5afeef9', narrow_sovereignty_prohibition).
narrative_ontology:cs_drift_state('21d53190-6635-4661-bf48-025ce5afeef9', post_artemis_consolidation, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('21d53190-6635-4661-bf48-025ce5afeef9', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_flag_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_operators).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, artemis_accords_signatories).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_developing_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, emerging_space_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, emerging_space_nations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, artemis_accords_signatories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and administer national statutes granting title to space resources extracted by licensed operators (United States 2015, Luxembourg 2017, United Arab Emirates and Japan 2019). Register operators under their flags, defend the narrow reading of Article II in UN forums, and recruit other states into the Artemis Accords, whose resource-utilization section restates the permissive position. They collect licensing fees, jurisdictional prestige, and first positioning in resource streams; their exit consists of rewriting or reinterpreting their own statutes, which they control.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_flag_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold licenses under flag-state regimes and plan missions for lunar water ice, regolith-derived construction material, and metal-rich asteroid material. Title to whatever they recover vests in them under flag-state law. Their costs are mission capital, licensing fees, and dependence on continued flag-state diplomatic protection of the reading; they can re-incorporate under another permissive flag if one statute turns hostile.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_operators, beneficiary,
    organized, biographical, mobile, global).

% Signed the US-drafted accords, gaining promised participation in a rules-based resource order and technical partnership with the drafter state. They accept the permissive reading as written by someone else and bear the cost of diminished standing in the rival common-heritage camp; several joined after bilateral cooperation was made contingent on signature.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, artemis_accords_signatories, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, artemis_accords_signatories, payer).

% Party to the Outer Space Treaty but without independent launch or resource-recovery capability. They bear uncompensated loss of the common-heritage position they endorsed in the 1979 Moon Agreement, watch resource streams allocate by capability before any multilateral regime convenes, and hold no seat in any flag-state licensing decision. Exiting the treaty system would cost them standing in all space governance; their leverage runs through bloc statements in the COPUOS Legal Subcommittee.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_developing_states, payer,
    organized, generational, trapped, global).

% Operate growing programs (lunar landers, sample return, planned resource demonstrations) and intend to enter the resource order. They arrive behind first movers whose sites and supply chains are consolidating, and to participate they must accept flag-state-drafted norms rather than co-author them. They gain a path to future resource access that a conservation reading would place behind a collective regime, and pay for it with reduced say over the rules.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, emerging_space_nations, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, emerging_space_nations, beneficiary).

% Legal scholars, NGO lawyers, and delegates aligned with the Moon Agreement's common-heritage article. They argue in COPUOS sessions and journals that unilateral title statutes breach the treaty's object and purpose, and draft model regime texts that no capable state has adopted. Leaving the debate would abandon the only institutional venue where their position is heard; their professional standing is bound to the common-heritage argument.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, space_resource_commons_advocates, excluded,
    moderate, biographical, identity_locked, global).

% The UN Committee on the Peaceful Uses of Outer Space Legal Subcommittee hosts the annual debate among the readings, produces non-binding guidelines, and runs a working group on space resource activities without a mandate to decide. It collects positions from every seat and shapes norms but holds no enforcement power over flag-state statutes.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, copuos_legal_committee, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_operators).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bars sovereign territorial claims over celestial bodies, preventing great-power annexation races and inter-state territorial conflict, and supplies a stable legal backdrop under which all space activity by all parties proceeds.
% TRANSFER_FUNCTION: Moves title to recovered space resources (water ice, regolith derivatives, metal-bearing material) from an unallocated common pool to flag-state-licensed private operators; moves licensing and registry authority to a handful of national governments; moves de facto site decision rights to whoever arrives with capability first.
% ABSENT_VOICES: States without space programs and future entrants hold no seat in any flag-state licensing decision where titles are actually granted; commons-heritage advocates attend COPUOS but command no vote over national statutes; Moon Agreement parties see their instrument sidelined without ever having it formally rejected. Unanimity in favor of the permissive reading arises within flag-state and accord circles precisely because the seats that would object were never in those rooms.
% DISAPPEARANCE_RATIONALE: If the permissive reading ceased to operate overnight (extraction barred pending an international regime), licensed operations would halt, four national title statutes would be void in effect, committed mission capital would freeze, and the Artemis framework's resource section would collapse; conversely, if the reading hardened into full territorial appropriation, the treaty's core bargain would dissolve. Either way the arrangements of every named seat reorganize around the change.
% FOUNDING_PROBLEM: Two-layered genealogy. The 1967 kernel was built to stop Cold War territorial extension onto the Moon and planets - to prevent either superpower converting landing sites into national territory. The extraction-permissive reading was authored much later (2015-2019 national statutes) to solve a newer problem: investor uncertainty about whether recovered material could be owned at all, which was freezing private capital for resource missions.
% FOUNDING_PROBLEM_CORROBORATION: Space-law historians and COPUOS delegations from non-benefiting states attest the original appropriation-race problem is dormant - no state has pressed a territorial claim since 1967. Industry legislative testimony and flag-state parliamentary records attest the investment-certainty problem was real and is being addressed. Moon Agreement parties and commons-heritage scholars dispute that unilateral statutes solve anything, holding that the legitimacy question the reading sidesteps remains live. No attestation of either layer comes from the beneficiary set alone.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.72: the arrangement transfers unallocated common-pool value to capability holders with no offsetting obligation, and the transfer compounds as sites and supply chains consolidate ahead of any regime. Suppression 0.70: persistence depends on active maintenance - flag-state statutes must keep being enacted and defended, the rival reading must keep being outmaneuvered in COPUOS, and accords accession must keep expanding the norm-locking coalition; suppression here is structural (capability gap, registry gatekeeping) rather than coercive in the interpersonal sense. Theater 0.38: a real but minority share of activity is performative - 'safety zones' framed as flight-safety measures that function as site-reservation instruments, sustainability branding over uncompensated enclosure - and that share grows as the justification load increases. Accessibility_collapse 0.48: alternatives do not fully collapse - the Moon Agreement route and regime-building remain formally available, and capable actors face no closure at all - but for capability-poor states the practical alternative is blocked, which is why the value sits mid-range rather than at either pole. Resistance 0.58: sustained G77 bloc statements, Moon Agreement advocacy, working-group fights, and a scholarly counter-literature meet the reading continuously without displacing it. Temporal series run on one shared grid (T=0..15, mapping approximately 2015-2030: T0 is the CSLCA's enactment) with every tracked metric authored at every point; T0-T9 are observed, T12-T15 are marked projected. All three series rise monotonically - no oscillation is modeled, and none is claimed. Rising base_extractiveness models enclosure accumulation; rising suppression_requirement models the maturing enforcement-and-legitimacy machinery (statute proliferation, accord expansion, diplomatic defense) the reading now requires; rising theater_ratio models the shifting justification mix.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the flag-state seat the arrangement is lawful facilitation it authored and administers - coordination it built, priced at diplomatic maintenance. From the operator seat it is opportunity: title without bearing the arrangement's costs beyond fees, with re-flagging mobility. From the trapped developing-state seat the identical structure operates as capability-gated exclusion with no compensation and no seat at the licensing table. Same treaty membership, same nominal legal order, divergent computed types - driven by declared role, power, and exit atoms, not by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Operators sit nearest the beneficiary end: they receive vested title and hold arbitrage-grade exit (re-incorporation under another permissive flag). Flag states sit low but above operators: they collect fees and positioning yet bear the arrangement's diplomatic maintenance costs. Artemis signatories sit mid-low: real access promises, real subordination costs. Developing states sit near the target end, amplified by trapped exit - they cannot leave the treaty system without losing all standing and cannot buy capability on any relevant timescale. Emerging nations sit mid-high with a dual declaration: they pay today in rule-taking and stand to gain tomorrow if they reach capability. Commons advocates are excluded rather than coordinated - their exclusion from decision seats is part of what the enforcement machinery sustains. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit atoms reproduces these relationships without correction, and the schema's override surface is keyed by power atom, which would collide across the two moderate seats rather than separate them.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical misreadings. Reading the arrangement as pure coordination (rope) erases the enforced asymmetry: the sovereignty bar genuinely benefits everyone, but the resource-title carve-out is maintained against identifiable objection and delivers nothing to those locked out. Reading it as pure extraction (snare) erases the surviving coordination function: even the paying seats retain the no-territorial-claims guarantee, which is why they stay inside the treaty system rather than exiting. The R5 interview carries the obsolescence signal: the kernel's founding problem (great-power territorial race) is corroborated dead from outside the beneficiary set, while the reading persists serving a newer commercial-enclosure function whose adequacy the parties contest - hence founding_problem_status contested paired with disappearance_verdict world_rearranges, flagging the repurposing without asserting a settled zombie verdict. Identity-lock appears on the excluded seat: commons advocates are bound by professional identity fusion with the common-heritage doctrine; if that frame broke, the excluded seat would empty rather than flip, since no permissive-compatible position is available to them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_omega,
    'This story instantiates only the extraction_permissive reading of kernel ost_article_ii_non_appropriation. The commons_conservation sibling would move resource-recovery activity into the target set and re-author epsilon around recovery as such; the international_regime sibling would suspend both readings and relocate gatekeeping to a future multilateral body. Which reading should the corpus treat as operative?',
    'COPUOS working-group outcome, an ICJ advisory request, or state-practice consolidation measured as the count of national title statutes and accords accessions against Moon Agreement ratifications over time.',
    'Adopting commons_conservation converts this story''s beneficiary seats into targets and collapses the arrangement''s legitimacy claim; adopting international_regime replaces capability-gated access with negotiated access and moves the agenda-setter seat to the future regime. The disagreement is located in the semantic scope of ''appropriation'' in Article II.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_omega, conceptual, 'Committer structure: this constraint is one of three readings of the Article II kernel; sibling readings change the victim set, the agenda-setter seat, and epsilon.').

omega_variable(
    fait_accompli_reversibility,
    'Does resource-site consolidation - landed infrastructure, in-situ supply chains, accumulated safety-zone practice - become irreversible before any multilateral regime convenes?',
    'Track cumulative licensed operations, permanent lunar infrastructure counts, and regime-negotiation milestones on a common timeline; irreversibility is indicated when dismantling or reallocating consolidated sites would require consent of the consolidator.',
    'Irreversibility raises effective suppression of the regime path for excluded seats and pushes their computed classifications toward harder extraction categories; reversibility keeps the international_regime route live and caps the reading''s entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fait_accompli_reversibility, empirical, 'Whether enclosure hardens faster than regime formation.').

omega_variable(
    compensation_obligation_under_customary_law,
    'Does customary international law impose any benefit-sharing or compensation duty on resource recoverers toward non-capable states, or is the pool legally open (res nullius) pending contrary practice?',
    'State-practice and opinio-juris surveys; any adjudicated ruling touching space-resource title; COPUOS consensus texts on benefit sharing.',
    'An emergent duty would convert uncompensated enclosure into a violation stream and raise measured extractiveness; its confirmed absence validates the reading''s no-compensation structure as lawful and pins the asymmetry on the capability gap alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_obligation_under_customary_law, empirical, 'Existence and content of any customary compensation obligation.').

omega_variable(
    safety_zone_precedent_trajectory,
    'Will Artemis ''safety zone'' practice harden into de facto site control amounting to the appropriation the treaty text forbids - converting the permissive reading into its own violation?',
    'Compare declared safety-zone extents and durations against interference incidents, third-party protest records, and whether any zone outlasts the hazard that justified it.',
    'Hardening vindicates the conservation sibling retroactively, undermines the reading''s textual-fidelity claim, and raises suppression for any actor approaching consolidated sites; dissolving zones after hazard clearance supports the permissive framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_zone_precedent_trajectory, empirical, 'Whether safety zones become functional territory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_art2_perm_tr_t0, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0, 0.14).
narrative_ontology:measurement(ost_art2_perm_tr_t3, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 3, 0.19).
narrative_ontology:measurement(ost_art2_perm_tr_t6, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 6, 0.24).
narrative_ontology:measurement(ost_art2_perm_tr_t9, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 9, 0.29).
narrative_ontology:measurement(ost_art2_perm_tr_t12, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ost_art2_perm_tr_t15, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 15, 0.38).

% Extraction over time
narrative_ontology:measurement(ost_art2_perm_be_t0, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ost_art2_perm_be_t3, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 3, 0.57).
narrative_ontology:measurement(ost_art2_perm_be_t6, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(ost_art2_perm_be_t9, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 9, 0.67).
narrative_ontology:measurement(ost_art2_perm_be_t12, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(ost_art2_perm_be_t15, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 15, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ost_art2_perm_su_t0, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(ost_art2_perm_su_t3, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 3, 0.51).
narrative_ontology:measurement(ost_art2_perm_su_t6, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(ost_art2_perm_su_t9, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 9, 0.61).
narrative_ontology:measurement(ost_art2_perm_su_t12, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(ost_art2_perm_su_t15, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_international_regime).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Article II non-appropriation' per the epsilon-invariance principle. The label conflates three structurally distinct claims: this file authors the extraction_permissive reading alone (epsilon 0.72, referent = the standing flag-state-licensed arrangement, victims = capability-excluded states); the commons_conservation sibling authors the same text as barring de facto appropriation through recovery (its epsilon assesses recovery-as-appropriation and its victim set centers the resource pool itself); the international_regime sibling authors the deferral structure (epsilon indeterminate pending regime formation, gatekeeping relocated upstream). Family edges run from this story to both siblings: the permissive reading's fait accompli structurally pressures the regime sibling's bargaining baseline and directly contradicts the conservation sibling's core premise. Upstream/downstream: the 1967 text is upstream of all three; this reading is downstream of the text and upstream of the regime sibling's negotiating conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
