% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Polytheistic Divine Legitimacy Through Priestly Interpretation
 *   domain: religious/political/economic
 *
 * SUMMARY:
 *   In the New Kingdom of Egypt, divine legitimacy flowed through the
 *   priesthood's interpretive authority over a polytheistic cosmology
 *   centered on Amun-Ra as supreme patron. The pharaoh required priestly
 *   coronation and continuous validation; temples accumulated land and labor
 *   obligations; regional practices were accommodated so long as they
 *   deferred to the central interpretive hierarchy. This reading frames the
 *   constraint as a genuine coordination solution—unifying regional cults
 *   under a shared cosmological framework and a distributed but hierarchical
 *   priesthood—while simultaneously recognizing it as extractive: the
 *   priesthood used its interpretive monopoly to accumulate resources and
 *   suppress rival spiritual authorities. The constraint is CLAIMED as
 *   tangled_rope (genuine coordination plus asymmetric extraction) and the
 *   metrics reflect this reading: extraction and suppression both substantial
 *   and rising through the interval, theater ratio moderate (real ritual
 *   function intertwined with power consolidation).
 *
 * KEY AGENTS:
 *   - Amun-Ra priesthood (Thebes): institutional agenda-setter, defines canonical interpretation, collects temple revenue and labor — d near 0.0 (beneficiary)
 *   - Pharaonic throne: powerful payer and beneficiary, requires priestly validation but gains unified religious authority — d near 0.5 (symmetric cost-benefit)
 *   - Regional temple networks: organized payers and constrained beneficiaries, accommodate local practice within priestly hierarchy — d near 0.6 (moderate target)
 *   - Unordained practitioners: powerless victims, identity-locked, intermittently suppressed as heretics — d near 1.0 (full target)
 *   - Common populace: powerless beneficiaries (access to ritual, cosmological coherence) and trapped payers (cannot articulate alternatives) — d near 0.55 (symmetric in role, constrained in exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.71).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Polytheistic Divine Legitimacy Through Priestly Interpretation").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "religious/political/economic").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '55129aa9-9bed-42a0-a958-4893932c7488').
narrative_ontology:cs_kernel_codification('55129aa9-9bed-42a0-a958-4893932c7488', distributed).
narrative_ontology:cs_authority_grounding('55129aa9-9bed-42a0-a958-4893932c7488', lineage).
narrative_ontology:cs_interpretation_layer_present('55129aa9-9bed-42a0-a958-4893932c7488').
narrative_ontology:cs_reading_relation('55129aa9-9bed-42a0-a958-4893932c7488', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('55129aa9-9bed-42a0-a958-4893932c7488', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('55129aa9-9bed-42a0-a958-4893932c7488', foundational, polytheistic_cosmology_necessary).
narrative_ontology:cs_axiom_status(polytheistic_cosmology_necessary, holdable).
narrative_ontology:cs_axiom_grounding('55129aa9-9bed-42a0-a958-4893932c7488', polytheistic_cosmology_necessary, conventional).
narrative_ontology:cs_axiom('55129aa9-9bed-42a0-a958-4893932c7488', foundational, distributed_hierarchical_priesthood_legitimacy).
narrative_ontology:cs_axiom_status(distributed_hierarchical_priesthood_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('55129aa9-9bed-42a0-a958-4893932c7488', distributed_hierarchical_priesthood_legitimacy, deontological).
narrative_ontology:cs_reference_frame('55129aa9-9bed-42a0-a958-4893932c7488', unified_polytheistic_cosmology_with_amun_supremacy).
narrative_ontology:cs_drift_state('55129aa9-9bed-42a0-a958-4893932c7488', ramesside_priestly_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('55129aa9-9bed-42a0-a958-4893932c7488', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, priestly_establishment).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, major_temples).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_cult_clergy).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_autonomous_authority).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_centers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, unordained_spiritual_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_throne).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, scribal_administrative_class).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, common_populace).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_throne).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, regional_temple_networks).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, unordained_practitioners).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, polytheistic_cosmology_necessity).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, hierarchical_priesthood_legitimacy).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, amun_ra_supreme_patron).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the authoritative interpretation of Amun-Ra's will and the broader polytheistic cosmology. Controls the scribal tradition, interprets omens and oracles, performs the daily rituals that legitimize pharaonic rule, and validates succession. Collects temple revenue, land grants, and labor obligations from the state in exchange for this interpretive monopoly. Can broker competing pharaonic claims through strategic revelation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_ra_priesthood, agenda_setter,
    institutional, generational, arbitrage, national).

% Requires priestly coronation and continuous validation to govern legitimately. Bears the cost of temple endowments, ritual obligations, and the constraint that pharaonic will must align with priestly interpretation of divine intent. Gains the coordination benefit of unified divine sanction and the institutional support the priesthood provides in suppressing rival claimants. Cannot rule without priestly confirmation; priesthood cannot rule without pharaonic enforcement.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_throne, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_throne, beneficiary).

% Conduct local worship and maintain regional variant practices honoring Amun-Ra alongside local deities. Must align their theological claims with the central priestly interpretation or face suppression. Pay tribute to the Amun-Ra priesthood at Thebes and accept its hermeneutical authority. Benefit from the framework's accommodation of local practice so long as they do not challenge the hierarchy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_temple_networks, payer,
    organized, generational, constrained, regional).

% Gain literacy, status, and institutional position through training in the priestly scribal tradition and the cosmological system it upholds. Serve as intermediaries between priestly authority and pharaonic administration, translating divine will into administrative decree. Their mobility comes from the transferability of literacy across institutions, but their careers depend on maintaining the cosmological framework they learned.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, scribal_administrative_class, beneficiary,
    moderate, biographical, mobile, national).

% Conduct healing, divination, and household magic outside the official priestly hierarchy. Are intermittently suppressed—labelled as charlatans, sorcerers, or heretics—when their practice contradicts or competes with authorized priesthood interpretations. Their spiritual identity and livelihood are bound to their practice; they cannot easily relocate or adopt alternative professions. Face social stigma and material hardship when suppressed.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, unordained_practitioners, payer,
    powerless, immediate, identity_locked, local).

% Access the priesthood's calendar of festivals, healing rituals, and intercession with the divine through which they understand their place in the cosmos and seek favor for their crops, households, and deaths. Cannot articulate or enforce alternative cosmologies without risking spiritual and social ostracism. Trapped within the framework; the priests control access to divine interpretation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, common_populace, beneficiary,
    powerless, immediate, trapped, local).

% Claim direct revelation from a single supreme deity (Aten, or others) and challenge the polytheistic cosmology. Are systematically excluded from the priestly establishment or suppressed when they gain pharaonic patronage. Their challenge is to the kernel itself—the validity of polytheism and distributed priestly interpretation—so the machinery exists partly to keep them out.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, rival_monotheistic_visionaries, excluded,
    moderate, biographical, trapped, national).

% Conduct domestic religious practice, honoring household gods and deceased ancestors, often with minimal reference to the official cosmology. Are not formally excluded but exist in a liminal space: their practice is tolerated so long as it does not challenge priestly authority. If they begin to teach alternative cosmologies, they risk suppression.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, household_cult_practitioners, excluded,
    powerless, immediate, identity_locked, local).

% Encounter the Egyptian polytheistic system as one cosmology among many, often seeking to integrate Egyptian deities into their own pantheons or negotiate trade under Egyptian religious authority. View the system as a functional legitimacy mechanism rather than a truth claim. Can exit to other legitimacy frameworks and occasionally expose the contingency of the Egyptian reading.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, foreign_merchants_and_diplomats, observer,
    organized, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, amun_ra_priesthood).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the Egyptian state under a shared cosmological framework in which multiple deities operate according to cosmic law (Ma'at), with Amun-Ra as the supreme organizing principle. Priestly interpretation provides a canonical reading that resolves theological disputes, validates pharaonic succession, and coordinates regional temples under a single religious authority rather than allowing fragmentation into competing cult centers.
% TRANSFER_FUNCTION: Moves labor obligations, land grants, and state resources from pharaonic treasury to the priesthood and temples, in exchange for continued validation that the pharaoh embodies and executes divine will. Moves spiritual authority and interpretive control from potential rival prophets or regional visionaries to the established priestly establishment. Moves social compliance from the populace to the priesthood through the promise of ritual intercession and cosmological coherence.
% ABSENT_VOICES: Rival monotheistic visionaries (those claiming direct revelation of a single deity), unordained practitioners (healers and diviners operating outside the priestly hierarchy), and household-based religious practitioners (who might articulate alternative cosmologies rooted in domestic practice) are structurally excluded or suppressed. They would challenge the necessity of priestly interpretation and the exclusivity of polytheistic orthodoxy.
% DISAPPEARANCE_RATIONALE: If the constraint and its enforcement vanished, pharaonic succession would lose its primary legitimacy mechanism; regional temples would reorganize around competing authority structures; unordained practitioners and alternative cosmologies would proliferate; the state would need to establish new bases for rule (military force, bureaucratic efficiency, or new religious systems) or collapse into fragmentation.
% FOUNDING_PROBLEM: Early dynastic Egypt faced the challenge of unifying multiple regions under a single pharaonic authority while accommodating the diverse local deities and practices already established in each region. A polytheistic framework with a supreme patron (Amun-Ra) and a distributed but hierarchical priestly interpretation permitted the pharaoh to claim universal authority without destroying regional religious identity.
% FOUNDING_PROBLEM_CORROBORATION: Priestly sources and pharaonic inscriptions attest the founding problem as the necessity of religious unity and regional accommodation—sources sympathetic to the system. Comparative historians and archaeologists who examine evidence of regional cult suppression and priestly authority accumulation suggest the founding problem was partly solved and partly replaced by extractive consolidation of power. No external corroboration from outside the benefiting parties exists for the claim that polytheistic coordination was necessary (alternative frameworks existed in neighboring civilizations).
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction of 0.62 reflects the substantial flow of resources from pharaonic and regional centers to temples, plus the opportunity cost of the pharaoh being constrained by priestly validation rather than governing autonomously. Suppression of 0.71 reflects active enforcement against unordained practitioners and competing visionaries; the priesthood maintains its interpretive monopoly through institutional power and the threat of delegitimization. Theater ratio of 0.48 indicates that roughly half the observed priestly activity is genuinely functional (daily temple rituals, genuine cosmological arbitration) and half is performative (elaborate display, the aesthetic assertion of authority that serves no coordination function beyond signaling dominance). The measurement series on a shared time grid show rising extraction and suppression through the 18th-19th dynasties (1550–1070), peaking in the mid-Ramesside period (1160 BCE) as priestly land holdings reached ~30% of arable Egypt, then declining slightly by the end of the 20th dynasty as pharaonic power recovered slightly and priestly authority began to fragment into competing regional priesthoods. This trajectory is consistent with extractive drift: the constraint's founding coordination function (unifying regions under shared cosmology) was achieved early; the subsequent rise in extraction and suppression reflects the priesthood's consolidation of power beyond what coordination requires.
 *
 * PERSPECTIVAL GAP:
 *   From the pharaonic seat: the arrangement is a necessary coordination mechanism that legitimizes rule and coordinates regional temples; the pharaoh accepts ritual constraint as the price of unified authority. From the priestly seat: the arrangement justifies and conceals the systematic accumulation of wealth and power; the priesthood extracts because it can. From the regional temple seat: the arrangement accommodates local practice but subordinates it; suppression appears only when local priests assert independence. From the unordained practitioner seat: the arrangement is pure extraction and suppression—a monopoly on spiritual legitimacy that forecloses alternative healing and divination practices. The engine should compute different types at each seat: the pharaonic seat might compute rope or tangled_rope (genuine coordination, asymmetric but negotiated), while the powerless practitioner seat computes snare (extraction, suppression, trapped exit). The authored claim (tangled_rope) reflects the view from the pharaonic and priestly beneficiary seats; a view from the suppressed unordained seat would likely claim snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Amun-Ra priesthood: declared beneficiary, institutional power, arbitrage exit (can shape which pharaohs rise by controlling validation) → d near 0.0 to 0.15 (full beneficiary). Pharaonic throne: declared beneficiary (gains unified authority) and payer (bears constraint and resource cost), powerful but constrained exit (cannot rule without priestly approval) → d near 0.45 to 0.55 (symmetric). Regional temples: declared payers (pay tribute, accept hierarchy), organized but constrained exit (can be suppressed, dependent on central validation) → d near 0.55 to 0.65 (moderate target). Unordained practitioners: declared victims (suppressed, excluded), powerless and identity-locked (cannot exit without abandoning spiritual identity) → d near 0.85 to 1.0 (full target). Common populace: beneficiaries (access ritual) and trapped payers (cannot articulate alternatives), powerless and identity-locked → d near 0.50 to 0.60 (symmetric in benefit/cost, constrained in exit so effectively tilted toward target). No overrides needed; the structural derivation from beneficiary/victim + exit captures the reading's dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT yet exhibit mandatrophy (the founding problem is still contested as live or dead by different parties), but the measurement trajectory shows the preconditions for it: extraction rising beyond coordination requirement suggests the founding problem is being replaced by pure rent collection. If the trajectory continued, we would expect to see theater_ratio rising to above 0.65–0.70, signaling that priestly activity is becoming mostly performative. The data at interval-end (1070 BCE) shows theater_ratio = 0.48 and suppression still actively engaged (0.71), which is consistent with a constraint that is substantially extractive but not yet purely theatrical—the priesthood still enforces the cosmological interpretation because it needs to maintain the framework to justify the extraction. The constraint should remain classified as tangled_rope so long as the coordination function (unifying cosmologies, validating succession) is genuinely performed, even as extraction rises. It would become piton only if measurement showed rising theater_ratio and falling active enforcement—that is, performance without functional suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    polytheism_necessity_vs_imperialism,
    'Was the polytheistic framework with Amun-Ra supremacy necessary to coordinate regional Egypt, or was it a post-hoc rationalization for priestly power consolidation after military unification?',
    'Comparative analysis of how other ancient multi-region empires achieved coordination (Mesopotamian city-states, Hittite confederacy, Indian subcontinent) without requiring a single supreme deity; archaeological evidence of whether priestly authority accumulated before or after regional military conquest.',
    'If polytheism was necessary, the coordination reading holds and extraction is legitimate cost. If polytheism was post-hoc, the constraint is snare (extractive consolidation dressed in cosmological language). This is the reading''s core vulnerability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(polytheism_necessity_vs_imperialism, empirical, 'Whether polytheistic coordination was structurally necessary or a cover story for priestly accumulation.').

omega_variable(
    priestly_interpretive_distribution_actual_vs_claimed,
    'Was priestly interpretive authority actually distributed across regional priesthoods (as claimed), or was it monopolized by Thebes and exercised through coercive hierarchy?',
    'Archaeological evidence of regional priestly autonomy: comparative analysis of theological texts from different temples showing variation or convergence with Thebes orthodoxy; evidence of regional priesthoods that resisted central interpretation and their suppression or accommodation.',
    'High actual distribution would support this reading as genuine coordination with multiple interpreters. Low distribution (Thebes monopoly enforced coercively) would move the constraint toward snare and weaken the ''distributed authority'' axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_interpretive_distribution_actual_vs_claimed, empirical, 'The actual degree of distributed vs. monopolized priestly authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of unordained practitioners structural (external coercion by priestly institutional power) or internalized (practitioners themselves believe in the priesthood''s exclusive legitimacy)?',
    'Historical evidence of post-suppression trajectories: if unordained practitioners abandon their practice entirely after institutional pressure and do not return when pressure reduces, suppression is largely internalized (the practitioner accepts the priestly frame as legitimate). If practitioners return or resist whenever coercive pressure weakens, suppression is structural.',
    'If internalized, the effective suppression is higher than measured (the target carries the frame with them). If structural, the suppression metric accurately captures active institutional coercion. This affects the classification of unordained practitioners from ''snare'' to ''identity-captured constraint'' (which is still extractive but modeled differently).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression of competing spiritual authorities.').

omega_variable(
    atenist_foreclosure_vs_coexistence,
    'Does this polytheistic reading logically foreclose the atenist monotheistic reading (one cannot hold both in the same framework), or do they coexist as competing readings held by different parties?',
    'Historical analysis of whether atenism was ever integrated into polytheistic cosmology (Aten as one god among many, or as an aspect of Amun-Ra) or whether it always claimed to replace polytheism entirely. If integration was possible, readings coexist; if not, they foreclose.',
    'If they foreclose, the kernel contest is a zero-sum replacement (one framework must win). If they coexist, both readings are live and the kernel permits genuine pluralism. This affects network classification: forecloses implies sibling constraints are direct competitors for legitimacy; coexists implies they occupy different institutional seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atenist_foreclosure_vs_coexistence, conceptual, 'Whether the polytheistic and monotheistic readings are logically incompatible or simultaneously holdable.').

omega_variable(
    folk_syncretism_integration_vs_suppression,
    'Did the priestly establishment genuinely accommodate folk syncretistic practices (household gods, local spirits), or did it tolerate them only as long as they remained subordinate and non-threatening?',
    'Archaeological evidence of household ritual spaces and their relationship to official cult centers; textual evidence of priestly pronouncements about folk practices (acceptance, prohibition, incorporation); evidence of suppression when folk practices challenged priestly orthodoxy.',
    'Genuine integration would support the claim that this reading accommodates multiple legitimacy sources. Conditional tolerance (suppress when threatening) would shift the reading toward snare: the folk reading is excluded in fact, even if permitted in form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_syncretism_integration_vs_suppression, empirical, 'The actual degree of integration vs. conditional suppression of folk spiritual practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 1550, 1070).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1550, 0.32).
narrative_ontology:measurement_basis(divi_tr_t1550, observed).
narrative_ontology:measurement(divi_tr_t1420, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1420, 0.39).
narrative_ontology:measurement_basis(divi_tr_t1420, observed).
narrative_ontology:measurement(divi_tr_t1290, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1290, 0.45).
narrative_ontology:measurement_basis(divi_tr_t1290, observed).
narrative_ontology:measurement(divi_tr_t1160, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1160, 0.5).
narrative_ontology:measurement_basis(divi_tr_t1160, observed).
narrative_ontology:measurement(divi_tr_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1070, 0.48).
narrative_ontology:measurement_basis(divi_tr_t1070, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1550, 0.48).
narrative_ontology:measurement_basis(divi_be_t1550, observed).
narrative_ontology:measurement(divi_be_t1420, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1420, 0.54).
narrative_ontology:measurement_basis(divi_be_t1420, observed).
narrative_ontology:measurement(divi_be_t1290, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1290, 0.6).
narrative_ontology:measurement_basis(divi_be_t1290, observed).
narrative_ontology:measurement(divi_be_t1160, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1160, 0.64).
narrative_ontology:measurement_basis(divi_be_t1160, observed).
narrative_ontology:measurement(divi_be_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1070, 0.62).
narrative_ontology:measurement_basis(divi_be_t1070, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1550, 0.52).
narrative_ontology:measurement_basis(divi_su_t1550, observed).
narrative_ontology:measurement(divi_su_t1420, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1420, 0.6).
narrative_ontology:measurement_basis(divi_su_t1420, observed).
narrative_ontology:measurement(divi_su_t1290, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1290, 0.68).
narrative_ontology:measurement_basis(divi_su_t1290, observed).
narrative_ontology:measurement(divi_su_t1160, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1160, 0.75).
narrative_ontology:measurement_basis(divi_su_t1160, observed).
narrative_ontology:measurement(divi_su_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1070, 0.71).
narrative_ontology:measurement_basis(divi_su_t1070, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__amun_polytheistic_reading, 0.12).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_succession_validation__amun_path).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, temple_land_accumulation__new_kingdom).

% DUAL FORMULATION NOTE:
% This constraint is the amun_polytheistic_reading of the divine_legitimacy_substrate kernel. The atenist_monotheistic_reading and folk_syncretistic_reading are sibling constraints instantiating the same kernel under different interpretive frameworks, producing different ε values and beneficiary structures. All three readings are linked via network.affects_constraints; they represent the contested legitimacy ground of New Kingdom Egypt. The polytheistic reading treats legitimacy as flowing through distributed but hierarchical priestly interpretation; the atenist reading treats it as flowing through pharaonic revelation of Aten (forecloses polytheism); the folk reading treats it as flowing through household/village practice (makes institutional priesthood optional). Empirically, all three coexisted in some form during the New Kingdom; this story represents one coherent reading of how the polytheistic framework functioned. The network edge to pharaonic_succession_validation__amun_path reflects the constraint's role in validating the pharaoh's right to rule; the edge to temple_land_accumulation__new_kingdom reflects the resource concentration that followed from priestly authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__amun_polytheistic_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
