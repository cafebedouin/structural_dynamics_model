% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: OST Article II Non-Appropriation (Extraction-Permissive Reading)
 *   domain: international_law/space_governance/commons
 *
 * SUMMARY:
 *   The Outer Space Treaty (1967) Article II states: 'Outer space, including
 *   the Moon and other celestial bodies, is not subject to national
 *   appropriation by claim of sovereignty, by means of use or occupation, or
 *   by any other means.' This reading—the extraction-permissive
 *   interpretation—understands the prohibition as constraining only SOVEREIGN
 *   territorial claims, not private ownership of extracted resources. A
 *   spacefaring state can authorize its corporations to extract water ice,
 *   rare-earth metals, or lunar regolith; the extraction transfers ownership
 *   to the operator without the state claiming sovereignty over the
 *   extraction site. This reading has become operational reality as space
 *   mining technology matured (2010–2035). The effect is a
 *   high-extractiveness constraint: technologically advanced states and their
 *   operators gain exclusive access to high-value resources while excluded
 *   states bear the structural cost of resource depletion and institutional
 *   precedent-setting. The founding problem (preventing Cold War celestial
 *   territorial claims) was solved by 1970; the extraction-permissive reading
 *   emerges decades later as a reinterpretation to enable profit-driven
 *   extraction once technology permitted it. Excluded states and
 *   commons-conservation advocates argue the reading breaches the Treaty's
 *   'common heritage of mankind' language and pre-empts the Article XI
 *   process for negotiating an international regime. The constraint operates
 *   as a snare because: (1) extraction outcomes cannot be reversed and create
 *   irreversible enclosure, (2) excluded states lack technological means to
 *   exit or establish competing claims, (3) the suppression is maintained
 *   through legal interpretation and fait accompli rather than explicit
 *   exclusion, and (4) the reading benefits specific actors (spacefaring
 *   states and their corporations) at asymmetric cost to excluded parties who
 *   have no compensation mechanism.
 *
 * KEY AGENTS:
 *   - technologically_advanced_spacefaring_states: Institutional agenda-setters; control the interpretation via capability and fait accompli; arbitrage exit (can extract unilaterally).
 *   - private_space_mining_corporations: Powerful beneficiaries; operate under flag-state authorization; mobile exit (can relocate operations between jurisdictions if one state revokes license).
 *   - technologically_excluded_states: Moderate organizational power; constrained exit (cannot build extraction capability at speed sufficient to pre-empt enclosure); bear structural cost of resource depletion.
 *   - global_south_nations: Organized payer cohort; understood the Treaty's 'common heritage' language as protecting their interests; face fait accompli (retroactive remedy unlikely).
 *   - unrepresented_future_generations: Powerless, trapped in civilizational time horizon; excluded from interpretation process; inherit depleted commons.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.82).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.71).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.82).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, snare).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "OST Article II Non-Appropriation (Extraction-Permissive Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_law/space_governance/commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, 'ea1b8792-af58-466e-9661-7d8c57f3a732').
narrative_ontology:cs_kernel_codification('ea1b8792-af58-466e-9661-7d8c57f3a732', fixed_text).
narrative_ontology:cs_authority_grounding('ea1b8792-af58-466e-9661-7d8c57f3a732', extraction).
narrative_ontology:cs_interpretation_layer_present('ea1b8792-af58-466e-9661-7d8c57f3a732').
narrative_ontology:cs_reading_relation('ea1b8792-af58-466e-9661-7d8c57f3a732', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('ea1b8792-af58-466e-9661-7d8c57f3a732', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('ea1b8792-af58-466e-9661-7d8c57f3a732', foundational, article_ii_constrains_sovereignty_only).
narrative_ontology:cs_axiom_status(article_ii_constrains_sovereignty_only, holdable).
narrative_ontology:cs_axiom_grounding('ea1b8792-af58-466e-9661-7d8c57f3a732', article_ii_constrains_sovereignty_only, conventional).
narrative_ontology:cs_axiom('ea1b8792-af58-466e-9661-7d8c57f3a732', foundational, private_ownership_permissible_absent_prohibition).
narrative_ontology:cs_axiom_status(private_ownership_permissible_absent_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('ea1b8792-af58-466e-9661-7d8c57f3a732', private_ownership_permissible_absent_prohibition, deontological).
narrative_ontology:cs_reference_frame('ea1b8792-af58-466e-9661-7d8c57f3a732', non_appropriation_via_sovereignty_only).
narrative_ontology:cs_drift_state('ea1b8792-af58-466e-9661-7d8c57f3a732', contemporary_extraction_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea1b8792-af58-466e-9661-7d8c57f3a732', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_space_mining_corporations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, technologically_excluded_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, global_south_nations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, unrepresented_future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control interpretation and enforcement of the extraction-permissive reading through demonstration of capability and fait accompli operations. Authorize private corporations to extract and own celestial resources. Defend the reading against renegotiation by citing the Treaty's language ('does not create property rights' as limiting only sovereign claims, not private ownership). Can exit by ceasing authorization, but constrained by domestic revenue incentives and market competition with other spacefaring states.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_spacefaring_states, agenda_setter,
    institutional, generational, arbitrage, universal).

% Operate under the extraction-permissive reading to extract and own celestial resources. Depend on the reading's persistence to maintain investment viability and property claims. Face no liability for environmental impact, depletion, or compensation to excluded states. Exit is available if a home state revokes authorization, but constrained by competition pressure and limited alternative jurisdictions (only spacefaring states can authorize operations).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_space_mining_corporations, beneficiary,
    powerful, biographical, mobile, universal).

% Lack the technical and financial capacity to launch extraction operations. Bear the structural cost of resource depletion and institutional precedent-setting (exclusion from future resource distribution). Under the extraction-permissive reading, have no claim to extracted resources, no compensation mechanism, and no seat in future regime negotiation. Exit is limited to diplomatic protest or attempting to mobilize a coalition for a counter-reading (the commons-conservation or international-regime reading). Cannot accumulate extraction capability fast enough to pre-empt enclosure.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, technologically_excluded_states, payer,
    moderate, generational, constrained, universal).

% Negotiated the Outer Space Treaty on the basis of 'common heritage of mankind' language (Preamble and Article XI) understood as protecting their interests in shared resources. Face a fait accompli: once resources are extracted and owned by spacefaring states and their operators, retroactive compensation is unlikely. Can form coalitions to oppose the reading and push for alternative interpretations (international-regime, commons-conservation), but constrained by the power asymmetry (spacefaring states control both interpretation and operations). Time horizon is generational because resource depletion accumulates across decades.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, global_south_nations, payer,
    organized, generational, constrained, universal).

% Cannot participate in the treaty interpretation process. Under the extraction-permissive reading, inherit a celestial environment depleted of high-value resources and no institutional mechanism to demand restoration or compensation. Trapped in the constraint by their absence from current decision-making. Can only attempt to overturn the reading through future political mobilization once they acquire agency, which requires a dramatic shift in international power distribution.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, unrepresented_future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, unrepresented_future_generations, excluded).

% A coalition of states (mainly excluded states and the global South) and NGOs advocating the commons-conservation reading. Structurally excluded from controlling the interpretation because spacefaring states have already begun operations. Fait accompli makes the conservation reading moot for already-extracted materials. Influence is limited to diplomatic pressure, scholarly advocacy, and attempts to negotiate a future international regime that would impose benefit-sharing on further extraction. Exit is available only if the coalition mobilizes sufficient political leverage to trigger treaty renegotiation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, common_heritage_coalition, excluded,
    organized, generational, constrained, universal).

% Institutional actors (UNOOSA, space law scholars, multilateral forum facilitators) analyzing whether Article II defers the appropriation question to a future international regime (parallel to Article XI's framework for the seabed regime). See the extraction-permissive reading as pre-empting regime negotiation by establishing de facto property rights before any compensation or benefit-sharing framework can be negotiated. Attempt to broker a multilateral framework that would govern future extraction, but constrained by spacefaring states' lack of incentive to renegotiate once beneficial operations are underway.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_regime_negotiators, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_spacefaring_states).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits resource extraction without requiring sovereign territorial claims, avoiding the coordinate-the-territory problem that plagued early space governance. Enables private actors to operate under flag-state authorization, decoupling resource use from frontier-conquest dynamics.
% TRANSFER_FUNCTION: Transfers ownership and economic benefit of extracted celestial resources from the global commons (nominally 'heritage of mankind') to spacefaring states and their licensed private operators. Transfers political authority over resource allocation from multilateral decision-making (Article XI's regime-negotiation framework) to individual flag-state discretion.
% ABSENT_VOICES: Technologically excluded states, the Global South coalition, and unrepresented future generations are structurally absent from the interpretation process. They did not author the reading and cannot exit it without retroactive treaty renegotiation. Unrepresented space-faring states (those lacking indigenous launch capacity) are absent from both the authority structure and the resource distribution; they depend on the reading being open and unilateral but face the risk of retroactive closure if a multilateral regime emerges.
% DISAPPEARANCE_RATIONALE: If this reading were repudiated and replaced by commons-conservation (prohibiting private ownership) or international-regime (deferring to multilateral framework), the space resource economy would shift fundamentally. Already-extracted resources would trigger retroactive compensation disputes and reallocation demands. New extraction would require multilateral approval or benefit-sharing commitments. Spacefaring states and private operators would lose unilateral authorization power. The institutional architecture of space governance would move from flag-state unilateralism to collective gatekeeping.
% FOUNDING_PROBLEM: The Outer Space Treaty needed to prevent Cold War-era territorial competition and celestial colonization. Article II's non-appropriation clause solved this by forbidding sovereign territorial claims. The foundational concern was that space would become a new frontier for great-power territorial conflict if left unconstrained.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing Cold War celestial territorial conflict) was substantially solved by the Treaty's entry into force in 1970 and has remained solved through 2035. No major spacefaring state has claimed sovereignty over any celestial territory or attempted to enforce territorial borders in space. The UN General Assembly, space law scholars independent of spacefaring states, and negotiators of subsequent agreements (e.g., Moon Agreement signatories) all attest that the non-appropriation norm has held and prevented territorial conflict. The extraction-permissive reading emerges decades after this problem is resolved, as a reinterpretation enabled by new technology and commercial interest. Spacefaring states frame the reading as solving a different problem (enabling resource use); excluded states and international-regime advocates dispute that enabling extraction was ever part of the Treaty's mandate. The foundational problem's resolution is corroborated by every major state's acceptance of non-territorial space governance for 50+ years.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts at near-zero in 1967 (the founding problem was prevention of territorial claims, which the Treaty accomplished) and rises sharply from 2010 onward as space mining technology matures and operations begin. The 2035 value of 0.82 reflects: (1) high asymmetry in access (technological gatekeeping), (2) no compensation mechanism for excluded states, (3) irreversible resource depletion, (4) enclosure via fait accompli. Suppression rises from zero to 0.71 because the constraint's persistence requires active defense against contrary interpretations (commons-conservation, international-regime) and resistance from excluded states. The defense takes the form of continued assertion of the reading through state practice and operational fait accompli. Theater rises to 0.42 because a growing share of enforcement activity is performative: spacefaring states cite the reading's 'permissive' character to distinguish it from territorial appropriation, but the practical effect (unilateral resource capture) resembles territorial enclosure. The coercion grid shows level-differential compression: at the structural level, alternatives collapse most sharply (0.68 by 2035) because the Treaty's text is fixed and reinterpretation is harder than creating new facts on the ground; at the individual level, collapse is less severe (0.58) because individual spacefaring corporations retain mobility (can shift between states). Resistance declines from 0.88 at signature (when the founding problem motivated near-unanimous support for Article II's non-appropriation clause) to 0.42 by 2035 (when fait accompli has shifted expectations). The measurement series show the constraint evolving from a genuine coordination problem (preventing Cold War territorial conflict) to an extractive arrangement as technology enables unilateral resource capture.
 *
 * PERSPECTIVAL GAP:
 *   From the advanced spacefaring state and corporation seats, the reading is a permissive interpretation of text that is genuinely ambiguous: Article II says 'no appropriation by claim of sovereignty' but does not explicitly forbid private ownership. From the excluded-state and global-south seats, the same text is understood as protecting the 'common heritage' against de facto appropriation via extraction. The engine should compute radically different types from these positions: from the spacefaring seat, the constraint may compute as coordination (enabling resource use without territorial conflict); from the excluded-state seat, it should compute as snare (asymmetric extraction under legal cover). The directionality derivation reflects this: spacefaring states have high d toward beneficiary (arbitrage exit, control of interpretation), while excluded states have high d toward target (constrained exit, no voice in interpretation, bear the asymmetric cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Technologically advanced spacefaring states are beneficiaries (d ≈ 0.1–0.2): they control the interpretation, authorize extraction, and collect the economic rents. Their exit is arbitrage-grade (they can always choose not to extract, but constrained by market competition and domestic revenue pressure). Private space mining corporations are beneficiaries but with mobile exit (d ≈ 0.15–0.25): they operate under state license but can relocate if one state revokes the license; competition pressure keeps them in the market. Technologically excluded states are targets (d ≈ 0.75–0.85): they have constrained exit (cannot build extraction capability fast enough to compete), cannot influence the interpretation, and bear the cost of resource depletion. The global South nations sit near the target end (d ≈ 0.72–0.80): they are organized (higher power than individual excluded states) but still constrained in exit (cannot extract at sufficient scale before enclosure is complete). Future generations are pure targets (d ≈ 1.0): trapped exit, no voice in current interpretation, inherit the depletion. The overrides should not be needed here because the structural data (beneficiary/victim, exit options, power) map cleanly to the directionality chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing Cold War celestial territorial conflict) was genuinely solved by Article II's entry into force and sustained by the decades-long absence of extraction operations. The extraction-permissive reading emerges AFTER the founding problem is resolved, as a reinterpretation to enable extraction. By 2020, the original coordination problem (preventing territorial annexation) is dead: no state seriously intends to claim the Moon as territory under international law. The reading's persistence depends entirely on extraction operations creating fait accompli enclosure and on spacefaring states defending the reading against renegotiation. This is the mandatrophy signature: the constraint's original justification (coordination to prevent territorial conflict) is obsolete, but the constraint persists as an extractive tool. The theater ratio's rise from 0.08 to 0.42 reflects the shift from genuine coordination (where the reading was incidental to preventing conflict) to performative maintenance (where the reading is actively defended because it permits extraction). The coercion grid shows this: structural resistance to the reading fell from 0.88 (near-unanimous support for non-appropriation at signature) to 0.42 (resistance from excluded states is now ineffectual), indicating that the constraint's legitimacy is no longer a function of its coordinating value but of fait accompli enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_intent_at_1967,
    'Was the extraction-permissive reading (private ownership without territorial claims) a defensible interpretation of Article II at the Treaty''s signing in 1967, or is it a retroactive reinterpretation enabled by technological capability and geopolitical shift?',
    'Scholarly analysis of negotiation records, state declarations at signature, and preparatory works (Vienna Convention Article 32); statements by original drafters on the intent of Article II''s silence on private ownership.',
    'If the reading was a live interpretation at 1967, it reflects a legitimate treaty ambiguity; if it is retroactive, the reading is a reinterpretation that should require amendment or consensus under VCLT Article 31.3(b). The latter classification strengthens the mandate-atrophy and legitimacy-erosion case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_intent_at_1967, empirical, 'Whether the extraction-permissive reading was intended at signature or is a later reinterpretation.').

omega_variable(
    commons_heritage_binding_character,
    'Is the ''common heritage of mankind'' language in the OST Preamble a binding normative constraint on resource appropriation, or merely aspirational rhetoric with no enforcement mechanism?',
    'ICJ advisory opinion or state practice survey on whether states treat common-heritage language as legally binding; analysis of whether UNCLOS Part XI (seabed regime with benefit-sharing) supersedes or clarifies the OST''s common-heritage obligation.',
    'If common-heritage language is binding, the extraction-permissive reading breaches a core Treaty obligation and requires remedy (compensation, benefit-sharing regime); if merely aspirational, the reading is consistent with the Treaty text. This directly determines whether the constraint operates as snare (breach under cover of permissive interpretation) or as a legitimate reading of an ambiguous commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_heritage_binding_character, conceptual, 'Whether the OST''s common-heritage language is binding or aspirational.').

omega_variable(
    article_xi_deferral_intent,
    'Does Article XI (which defers the question of an international regime for celestial resources to future agreement) logically entail that extraction should be prohibited until that regime is negotiated, or does it permit extraction pending regime negotiation?',
    'Comparative analysis with UNCLOS Part XI (which explicitly permits seabed mining pending regime entry into force); state practice on whether deep-sea mining proceeded before ISA became operational; drafting history of Article XI''s deferral language.',
    'If deferral entails prohibition, the extraction-permissive reading violates Article XI by pre-empting regime negotiation; if deferral permits extraction, the reading is consistent. The interpretation choice here determines whether this reading forecloses or coexists with the international_regime reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_xi_deferral_intent, empirical, 'Whether Article XI deferral entails extraction prohibition or extraction permission pending regime negotiation.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (legal barriers, military enforcement of flag-state claims, technological exclusion) or internalized (excluded states'' belief that they cannot challenge the reading, demoralization from decades of fait accompli)?',
    'Post-reading-shift trajectory: if a multilateral regime amendment suddenly opened negotiation, would excluded states mobilize rapidly (structural suppression) or remain passive (internalized suppression)? Survey of state archives and diplomatic statements on belief in the changeability of the reading.',
    'If internalized, the effective suppression is higher than the structural measure and would persist even after the legal reading changes; if structural, removing the reading would unblock excluded states'' agency. The distinction affects downstream assessment of whether a constraint-shift (e.g., adoption of international regime) would self-heal or require reconstruction of excluded states'' capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of excluded states'' resistance is structural or internalized.').

omega_variable(
    kernel_reading_boundary,
    'Is the extraction-permissive reading a defensible interpretation of Article II''s text, or does it require rewriting Article II to remove the ambiguity and affirmatively authorize private ownership (which would exceed interpretation and enter amendment)?',
    'Textual analysis under Vienna Convention Article 31 (ordinary meaning, context, object and purpose) and Article 32 (supplementary means). If the text can bear the extraction-permissive meaning without logical contradiction, it remains a reading; if the reading requires the text to be reworded to function, it crosses into amendment.',
    'If the reading is defensible under VCLT Article 31, the constraint operates as a legitimate interpretation within the committed framework; if it requires amendment, the reading is a reinterpretation that should be challenged under Article 27 (good faith performance). This affects the kernel classification and the integrity of the constraint-family structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the extraction-permissive reading is an interpretation of Article II or a functional amendment requiring renegotiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.0).
narrative_ontology:measurement_basis(ost__tr_t1967, observed).
narrative_ontology:measurement(ost__tr_t1990, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1990, 0.08).
narrative_ontology:measurement_basis(ost__tr_t1990, observed).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2010, 0.22).
narrative_ontology:measurement_basis(ost__tr_t2010, observed).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2020, 0.38).
narrative_ontology:measurement_basis(ost__tr_t2020, observed).
narrative_ontology:measurement(ost__tr_t2030, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2030, 0.4).
narrative_ontology:measurement_basis(ost__tr_t2030, projected).
narrative_ontology:measurement(ost__tr_t2035, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2035, 0.42).
narrative_ontology:measurement_basis(ost__tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.0).
narrative_ontology:measurement_basis(ost__be_t1967, observed).
narrative_ontology:measurement(ost__be_t1990, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement_basis(ost__be_t1990, observed).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement_basis(ost__be_t2010, observed).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(ost__be_t2020, observed).
narrative_ontology:measurement(ost__be_t2030, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2030, 0.78).
narrative_ontology:measurement_basis(ost__be_t2030, projected).
narrative_ontology:measurement(ost__be_t2035, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2035, 0.82).
narrative_ontology:measurement_basis(ost__be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.0).
narrative_ontology:measurement_basis(ost__su_t1967, observed).
narrative_ontology:measurement(ost__su_t1990, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement_basis(ost__su_t1990, observed).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement_basis(ost__su_t2010, observed).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(ost__su_t2020, observed).
narrative_ontology:measurement(ost__su_t2030, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement_basis(ost__su_t2030, projected).
narrative_ontology:measurement(ost__su_t2035, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2035, 0.71).
narrative_ontology:measurement_basis(ost__su_t2035, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1967, tn=2035
narrative_ontology:measurement(ost__grid_01, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(class), 1967, 0.05).
narrative_ontology:measurement(ost__grid_02, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(class), 2035, 0.72).
narrative_ontology:measurement(ost__grid_03, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(individual), 1967, 0.02).
narrative_ontology:measurement(ost__grid_04, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(individual), 2035, 0.58).
narrative_ontology:measurement(ost__grid_05, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(organizational), 1967, 0.08).
narrative_ontology:measurement(ost__grid_06, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(organizational), 2035, 0.62).
narrative_ontology:measurement(ost__grid_07, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(structural), 1967, 0.15).
narrative_ontology:measurement(ost__grid_08, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(structural), 2035, 0.68).
narrative_ontology:measurement(ost__grid_09, ost_article_ii_non_appropriation__extraction_permissive, resistance(class), 1967, 0.8).
narrative_ontology:measurement(ost__grid_10, ost_article_ii_non_appropriation__extraction_permissive, resistance(class), 2035, 0.48).
narrative_ontology:measurement(ost__grid_11, ost_article_ii_non_appropriation__extraction_permissive, resistance(individual), 1967, 0.75).
narrative_ontology:measurement(ost__grid_12, ost_article_ii_non_appropriation__extraction_permissive, resistance(individual), 2035, 0.35).
narrative_ontology:measurement(ost__grid_13, ost_article_ii_non_appropriation__extraction_permissive, resistance(organizational), 1967, 0.85).
narrative_ontology:measurement(ost__grid_14, ost_article_ii_non_appropriation__extraction_permissive, resistance(organizational), 2035, 0.55).
narrative_ontology:measurement(ost__grid_15, ost_article_ii_non_appropriation__extraction_permissive, resistance(structural), 1967, 0.88).
narrative_ontology:measurement(ost__grid_16, ost_article_ii_non_appropriation__extraction_permissive, resistance(structural), 2035, 0.42).
narrative_ontology:measurement(ost__grid_17, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(class), 1967, 0.08).
narrative_ontology:measurement(ost__grid_18, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(class), 2035, 0.82).
narrative_ontology:measurement(ost__grid_19, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(individual), 1967, 0.05).
narrative_ontology:measurement(ost__grid_20, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(individual), 2035, 0.75).
narrative_ontology:measurement(ost__grid_21, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(organizational), 1967, 0.1).
narrative_ontology:measurement(ost__grid_22, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(organizational), 2035, 0.78).
narrative_ontology:measurement(ost__grid_23, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(structural), 1967, 0.2).
narrative_ontology:measurement(ost__grid_24, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(structural), 2035, 0.85).
narrative_ontology:measurement(ost__grid_25, ost_article_ii_non_appropriation__extraction_permissive, suppression(class), 1967, 0.0).
narrative_ontology:measurement(ost__grid_26, ost_article_ii_non_appropriation__extraction_permissive, suppression(class), 2035, 0.74).
narrative_ontology:measurement(ost__grid_27, ost_article_ii_non_appropriation__extraction_permissive, suppression(individual), 1967, 0.0).
narrative_ontology:measurement(ost__grid_28, ost_article_ii_non_appropriation__extraction_permissive, suppression(individual), 2035, 0.65).
narrative_ontology:measurement(ost__grid_29, ost_article_ii_non_appropriation__extraction_permissive, suppression(organizational), 1967, 0.0).
narrative_ontology:measurement(ost__grid_30, ost_article_ii_non_appropriation__extraction_permissive, suppression(organizational), 2035, 0.68).
narrative_ontology:measurement(ost__grid_31, ost_article_ii_non_appropriation__extraction_permissive, suppression(structural), 1967, 0.0).
narrative_ontology:measurement(ost__grid_32, ost_article_ii_non_appropriation__extraction_permissive, suppression(structural), 2035, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.08).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, unclos_seabed_mining_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_common_heritage_alternative).

% DUAL FORMULATION NOTE:
% The OST Article II non-appropriation clause is read three distinct ways by different parties and produces three structurally different constraints. This constraint family decomposes one naturally-ambiguous treaty provision into three ε-invariant constraint stories: EXTRACTION_PERMISSIVE (high extraction, technological gatekeeping, fait accompli enclosure); COMMONS_CONSERVATION (low extraction, de facto appropriation is prohibited); INTERNATIONAL_REGIME (deferral to future multilateral regime negotiation, extraction pending regime entry into force). Each reading has its own beneficiary set, exit structure, and type classification. The family is linked via network.affects_constraints: this reading (EXTRACTION_PERMISSIVE) influences both sibling readings by establishing a de facto baseline through operations, making the CONSERVATION reading harder to implement retroactively and pre-empting the INTERNATIONAL_REGIME reading by creating facts before regime negotiation. The ε values remain independent: this reading assesses the standing arrangement (extraction-permissive governance) at 0.82; the conservation reading would assess the same physical arrangement (celestial resources) under a different constraint (no private appropriation) at ~0.05–0.15.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
