% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: OST Article II Extraction-Permissive Reading: National-Licensing Resource Regime
 *   domain: international law/space governance/commons
 *
 * SUMMARY:
 *   Article II of the 1967 Outer Space Treaty declares that outer space,
 *   including the Moon and other celestial bodies, 'is not subject to
 *   national appropriation by claim of sovereignty, by means of use or
 *   occupation, or by any other means.' The reading authored here holds that
 *   this bars only sovereign territorial claims: it does not reach private
 *   actors, and ownership of resources once extracted is a matter for
 *   national law. Since 2015 the reading has been made operative — the US
 *   Commercial Space Launch Competitiveness Act (2015), Luxembourg's
 *   resources law (2017), the UAE (2019) and Japan (2019) equivalents, and
 *   the Artemis Accords (2020) — so that extraction ventures are licensed,
 *   title to extracted material is conferred by flag-state statute, and each
 *   operation hardens the interpretation as accomplished fact ahead of any
 *   multilateral adjudication. The standing arrangement under contest — the
 *   referent of epsilon — is this national-licensing extraction regime as it
 *   actually operates: access gated by technological capability and
 *   flag-state legal recognition, no compensation mechanism for excluded
 *   states, enclosure accumulating by fait accompli rather than formal
 *   annexation. Claim and metrics are authored independently: the constraint
 *   is claimed as a tangled rope (a genuine investment-certainty function
 *   welded to asymmetric extraction), and the metrics describe that operation
 *   without being tuned to any classification target.
 *
 * KEY AGENTS:
 *   - spacefaring_commercial_states (US, Luxembourg, UAE, Japan): agenda-setter — enacts and administers the national legislation and bilateral accords that make the reading operative; institutional power, arbitrage exit via registry competition
 *   - commercial_space_resource_operators: primary beneficiary — holds flag-state title to extracted resources and priority access to prime sites; powerful, arbitrage exit
 *   - nonspacefaring_developing_states: primary target — capability-gated out of access, uncompensated; organized as a diplomatic bloc, trapped exit
 *   - moon_agreement_states_parties: target — common-heritage commitments devalued by contrary state practice; trapped by ratification
 *   - rival_spacefaring_states (Russia, China and ILRS partners): excluded from the legal-recognition network and contesting the reading; institutional power, constrained exit
 *   - copuos_legal_subcommittee: analytical observer — consensus-bound UN venue where the contest persists unresolved
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.76).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.62).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.76).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "OST Article II Extraction-Permissive Reading: National-Licensing Resource Regime").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international law/space governance/commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '6fe15af2-dfe7-44cb-a506-f8024cc36c72').
narrative_ontology:cs_kernel_codification('6fe15af2-dfe7-44cb-a506-f8024cc36c72', fixed_text).
narrative_ontology:cs_authority_grounding('6fe15af2-dfe7-44cb-a506-f8024cc36c72', extraction).
narrative_ontology:cs_interpretation_layer_present('6fe15af2-dfe7-44cb-a506-f8024cc36c72').
narrative_ontology:cs_reading_relation('6fe15af2-dfe7-44cb-a506-f8024cc36c72', ost_article_ii_non_appropriation__ost_article_ii_commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('6fe15af2-dfe7-44cb-a506-f8024cc36c72', ost_article_ii_non_appropriation__ost_article_ii_international_regime, influences).
narrative_ontology:cs_axiom('6fe15af2-dfe7-44cb-a506-f8024cc36c72', foundational, appropriation_requires_sovereign_claim).
narrative_ontology:cs_axiom_status(appropriation_requires_sovereign_claim, holdable).
narrative_ontology:cs_axiom_grounding('6fe15af2-dfe7-44cb-a506-f8024cc36c72', appropriation_requires_sovereign_claim, conventional).
narrative_ontology:cs_axiom('6fe15af2-dfe7-44cb-a506-f8024cc36c72', secondary, extracted_resources_ownable_under_flag_state_law).
narrative_ontology:cs_axiom_status(extracted_resources_ownable_under_flag_state_law, holdable).
narrative_ontology:cs_axiom_grounding('6fe15af2-dfe7-44cb-a506-f8024cc36c72', extracted_resources_ownable_under_flag_state_law, conventional).
narrative_ontology:cs_reference_frame('6fe15af2-dfe7-44cb-a506-f8024cc36c72', sovereignty_only_non_appropriation).
narrative_ontology:cs_drift_state('6fe15af2-dfe7-44cb-a506-f8024cc36c72', artemis_era_state_practice, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6fe15af2-dfe7-44cb-a506-f8024cc36c72', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, commercial_space_resource_operators).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_commercial_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, space_science_community).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, nonspacefaring_developing_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_states_parties).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, rival_spacefaring_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, space_science_community).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, article_ii_sovereignty_only_interpretation).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, national_licensing_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate prospecting and extraction ventures on the Moon and near-Earth asteroids under national licenses. Title to extracted material is conferred by flag-state statute, and the value of extracted resources and of priority access to prime sites accrues to them. Exit: they can reincorporate under any licensing jurisdiction and contract around most single-state rules; their capital is mobile across flag registries.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, commercial_space_resource_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% Enact and administer the national legislation that makes the reading operative — licensing regimes and resource-title statutes — and negotiate the bilateral accords that extend it. They collect registry fees, tax base, and industry attraction from hosting extraction ventures, and bear the diplomatic cost of defending the reading in multilateral fora. They can amend their own statutes at will and compete with one another for registries.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_commercial_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Ratified the treaty framework declaring outer space the province of all mankind but possess no independent extraction capability and no seat in the Artemis framework. They bear the enclosure of resources they could not reach themselves: common-pool value is committed to license-holders without compensation, and the window in which a multilateral allocation could have included them is closing. Their lever is bloc diplomacy in consensus-bound fora.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, nonspacefaring_developing_states, payer,
    organized, generational, trapped, global).

% Bound themselves to the common-heritage principle and to an eventual international regime for lunar resources. Each licensed operation conducted outside that framework devalues their commitment and sets contrary state practice. Exit: they are bound by ratification; their remedy is objection and precedent-preservation, not withdrawal from a practice they never joined.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_states_parties, payer,
    organized, generational, trapped, global).

% Spacefaring powers outside the Artemis framework, operating a counter-coalition lunar program. The legal-recognition network gates them: their operators would hold no recognized title under the emerging practice. They contest the reading diplomatically while building an alternative architecture; entering the framework would require accepting the reading they contest.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, rival_spacefaring_states, excluded,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, rival_spacefaring_states, payer).

% Gains mission opportunities, delivered instruments, and data volume from commercial activity, and depends on operators for access to targets. Bears the risk that extraction degrades scientifically pristine sites — permanently shadowed regions, intact stratigraphy — before they are studied. Exit is limited: they ride the same missions whose conduct they would constrain.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, space_science_community, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, space_science_community, payer).

% The UN forum where the meaning of the non-appropriation article is debated. Works by consensus, so the contest persists unresolved; it can surface objections and draft building blocks but cannot adjudicate the treaty text. It neither collects nor pays; it records.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, copuos_legal_subcommittee, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, commercial_space_resource_operators).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the property-status vacuum for celestial resources: by licensing extraction under national law and recognizing ownership of extracted material, the arrangement gives investors an enforceable asset, coordinates capital, technology, and flag-state oversight around extraction ventures, and supplies safety and transparency rules for operations. Stated without evaluation: it solves the 'who may own what once extracted' problem that the treaty's silence left open.
% TRANSFER_FUNCTION: Moves exclusive access to celestial-resource opportunities from an open commons — available in principle to all states — to operators holding flag-state licenses; moves the value of extracted material to private firms and to the registry and tax base of flag states; moves the costs of foreclosed multilateral options to states without extraction capability or framework membership.
% ABSENT_VOICES: The conservation and deferred-regime proponents — Moon Agreement parties and G77 delegations in the UN forum — are outside the bilateral-accord conversation where the reading is operationalized. Future generations hold no seat. The international regime contemplated for lunar exploitation never convened, and the reading proceeds as if that absence were consent.
% DISAPPEARANCE_RATIONALE: If the reading were repudiated overnight — ownership of extracted resources suddenly held unlawful — committed extraction ventures would lose their legal basis, flag-state licensing regimes would collapse, framework operations would stall, and deployed capital would be stranded. The cislunar economy has already rearranged around the reading's property rule.
% FOUNDING_PROBLEM: The treaty barred national appropriation but was silent on private actors and resource extraction; with no ownership rule, extraction investment was legally unbankable — no lender or investor could take a security interest in material whose title was uncertain.
% FOUNDING_PROBLEM_CORROBORATION: The vacuum itself is corroborated from outside the benefiting parties: UN Legal Subcommittee agenda debates — including from states that reject the permissive answer — and the independent Hague Space Resources Governance Working Group both acknowledge the legal uncertainty is real. But the framing of the vacuum as a problem that national legislation may unilaterally solve is attested only by the benefiting parties; outside parties attest the vacuum and dispute the unilateral remedy.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.76 at interval end) because the value flows are asymmetric: title and site priority accrue to license-holders under flag-state law, while the excluded majority bears the foreclosure of commons value with no compensation mechanism, and each operation narrows the space of future allocations. Suppression (0.62) is structural, not interpersonal: it is the capability gate plus the legal-recognition network (an operator holds recognized title only inside the flag-state system), reinforced by a bilateral-accord web and by the normative force that accomplished facts exert on treaty interpretation — no physical coercion is involved, and the suppression metric is authored as the raw structural property without any scope or power scaling, which the engine applies on its side. Theater ratio (0.42) reflects the full-compliance language in the enabling statutes and accords: real licensing, safety, and coordination work occurs, but a growing share of interpretive activity asserts fidelity to a text whose conservation reading the same instruments hollow out. Accessibility_collapse (0.48): alternatives — a multilateral regime, a common-heritage allocation, UN building blocks — remain live proposals, but each licensed operation raises the cost of adopting them, so alternatives are narrowing rather than collapsed. Resistance (0.58): bloc objections in the UN forum, explicit rejection of the accords by the counter-coalition, and the Moon Agreement parties' contrary practice are real but have not slowed enactment. The three measurement series share one time grid (2015, 2017, 2019, 2021, 2023, 2026); the 2026 points are projections, the rest observed. The rising suppression_requirement series is authored deliberately: the enforcement machinery — licensing offices, accord signatories, operational precedent — is being built up over the interval, so enforcement-capacity change is part of what this story tracks.
 *
 * PERSPECTIVAL GAP:
 *   From the flag-state and operator seats the arrangement presents as coordination: it solves a real hold-up problem (without an ownership rule no investor finances extraction), licensees enter voluntarily, and its safety and transparency functions are genuine. From the excluded-state seats the same structure presents as enclosure: a declared common province is being committed by those with capability, through law they did not make, with no compensation and no consent. The engine computes per-seat classifications from the structural data — the divergence between these experiences is the measurement the corpus exists to take, not an error to be reconciled. The science seat sits between: it experiences genuine benefit (mission access, data volume) and genuine cost (degradation of pristine sites) from the same structure, which is why it carries dual roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the operators (arbitrage exit pushes them toward the beneficiary pole — they can re-flag and contract around any single regime) and for the flag states, which collect registry rents and industry attraction. The science community, declared a beneficiary with a secondary payer position, sits moderately low rather than at the pole. Victim declarations drive high directionality for the capability-excluded developing states (trapped exit: they cannot extract, and their only lever is consensus-bound diplomacy) and the Moon Agreement parties (trapped by ratification). The structurally interesting target is the rival spacefaring states: institutional power, yet the legal-recognition gate binds them regardless of power — their exit is constrained (join the framework and accept the reading, or build a counter-architecture), so their high directionality is a product of the gate, not of weakness. Global spatial scope amplifies effective extraction for targets, because equitable access at planetary scale cannot be verified absent a regime. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Calling the arrangement pure extraction would erase the genuine coordination function: the legal vacuum was real, is corroborated from outside the benefiting parties, and some ownership rule was necessary for any extraction at all — the reading solved that problem, not merely covered for its solution. Calling it pure coordination would erase the asymmetric extraction: the same problem could have been solved multilaterally, and the unilateral solution's costs land on states with no seat at the table. Tangled rope holds both facts. On mandatrophy: the founding problem (investment certainty for nascent extraction) is live, so the arrangement is not yet a zombie; but the resolution condition is visible — if a multilateral regime is eventually negotiated, the national-licensing patchwork would persist past its function as a transitional fix, and mandatrophy would resolve with the patchwork as the atrophied remnant. The coalition check for the weaker victim seats: the developing-state bloc acts collectively in the UN forum, but the consensus rule and the absence of independent capability cap its leverage, so organized power does not convert into exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the ost_article_ii_non_appropriation kernel (the fixed Article II text); what would change structurally if a sibling reading were adopted instead?',
    'A multilateral adjudication of Article II''s meaning — a new treaty, an ICJ advisory opinion, or a COPUOS consensus declaration — would select among the readings; until then the readings persist as rival constraints held by different state coalitions.',
    'Under the commons_conservation sibling, this story''s licensed operators become violators and the victim set expands to the commons itself; under the international_regime sibling, neither this reading nor the conservation reading is authoritative and enforcement shifts to a not-yet-existing framework. The disagreement is located in whether ''appropriation'' reaches private extraction or only sovereign territorial claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one kernel, three readings; this file instantiates the extraction_permissive reading only.').

omega_variable(
    fait_accompli_reversibility,
    'Will accomplished extraction facts harden the reading beyond diplomatic reversal before any multilateral body adjudicates Article II''s meaning?',
    'Track whether COPUOS consensus, a new treaty, or an ICJ opinion emerges before extraction scales past demonstration missions; count licensed operations, extracted mass, and capital committed.',
    'If irreversible, suppression and accessibility_collapse rise and the constraint drifts toward pure extraction with the coordination function as cover; if a regime emerges first, the reading becomes one negotiable input among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fait_accompli_reversibility, empirical, 'Whether physical enclosure creates normative lock-in faster than diplomacy can respond.').

omega_variable(
    appropriation_textual_scope,
    'Does ''national appropriation'' in Article II, properly read, reach private actors exercising state-licensed extraction rights, or only sovereign territorial claims?',
    'Vienna Convention interpretive analysis combined with accumulating state practice and opinio juris; trajectory of scholarly and diplomatic consensus.',
    'If the conservation textual reading prevails, this constraint''s legal foundation fails and licensed title becomes contestable; if the permissive reading prevails, the conservation sibling is displaced within any single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriation_textual_scope, conceptual, 'Textual scope of ''appropriation'' — the load-bearing ambiguity the reading turns on.').

omega_variable(
    capability_gate_diffusion,
    'Will independent extraction capability diffuse to many states, diluting the capability gate, or concentrate within the flag-state coalition, hardening it?',
    'Count states conducting extraction-class missions over the next two decades; track launch, ISRU, and in-space supply-chain independence.',
    'Diffusion moves the arrangement toward open access in practice and lowers effective extraction on the excluded majority; concentration raises it and accelerates drift toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_gate_diffusion, empirical, 'Trajectory of the technological capability gate that structures access.').

omega_variable(
    compensation_mechanism_emergence,
    'Will any benefit-sharing or compensation mechanism for excluded states emerge — through accords amendments, COPUOS building blocks, or treaty?',
    'Observe COPUOS Legal Subcommittee outcomes and Artemis-framework revisions for benefit-sharing language with operational force rather than aspirational preambular text.',
    'Emergence would damp effective extraction on excluded states and pull the arrangement toward compensated coordination; continued absence hardens the no-compensation structure the expected delta names.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compensation_mechanism_emergence, empirical, 'Whether the no-compensation feature persists or is remedied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 2015, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(ost__tr_t2015, observed).
narrative_ontology:measurement(ost__tr_t2017, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2017, 0.28).
narrative_ontology:measurement_basis(ost__tr_t2017, observed).
narrative_ontology:measurement(ost__tr_t2019, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2019, 0.31).
narrative_ontology:measurement_basis(ost__tr_t2019, observed).
narrative_ontology:measurement(ost__tr_t2021, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2021, 0.35).
narrative_ontology:measurement_basis(ost__tr_t2021, observed).
narrative_ontology:measurement(ost__tr_t2023, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2023, 0.38).
narrative_ontology:measurement_basis(ost__tr_t2023, observed).
narrative_ontology:measurement(ost__tr_t2026, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(ost__tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement_basis(ost__be_t2015, observed).
narrative_ontology:measurement(ost__be_t2017, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2017, 0.52).
narrative_ontology:measurement_basis(ost__be_t2017, observed).
narrative_ontology:measurement(ost__be_t2019, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement_basis(ost__be_t2019, observed).
narrative_ontology:measurement(ost__be_t2021, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2021, 0.64).
narrative_ontology:measurement_basis(ost__be_t2021, observed).
narrative_ontology:measurement(ost__be_t2023, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2023, 0.7).
narrative_ontology:measurement_basis(ost__be_t2023, observed).
narrative_ontology:measurement(ost__be_t2026, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2026, 0.76).
narrative_ontology:measurement_basis(ost__be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(ost__su_t2015, observed).
narrative_ontology:measurement(ost__su_t2017, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2017, 0.36).
narrative_ontology:measurement_basis(ost__su_t2017, observed).
narrative_ontology:measurement(ost__su_t2019, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement_basis(ost__su_t2019, observed).
narrative_ontology:measurement(ost__su_t2021, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2021, 0.5).
narrative_ontology:measurement_basis(ost__su_t2021, observed).
narrative_ontology:measurement(ost__su_t2023, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2023, 0.56).
narrative_ontology:measurement_basis(ost__su_t2023, observed).
narrative_ontology:measurement(ost__su_t2026, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(ost__su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_international_regime).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Article II non-appropriation' is one kernel text with three structurally distinct readings, each authored as its own story with its own epsilon, victim set, and classification. This file is the extraction_permissive reading (high epsilon: capability-gated, uncompensated access; enclosure by accomplished fact). The commons_conservation sibling would classify large-scale extraction itself as the violation, with the commons and its would-be beneficiaries as the protected party; the international_regime sibling defers the question and suspends both rivals' authority. Edges: this reading forecloses the conservation reading within any single framework and exerts downstream pressure on the regime reading, because every licensed operation changes the baseline a future regime would have to accommodate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
