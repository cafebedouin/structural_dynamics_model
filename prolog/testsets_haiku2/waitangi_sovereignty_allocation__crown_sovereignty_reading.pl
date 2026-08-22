% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of the Treaty of Waitangi (English Article I)
 *   domain: constitutional/post-colonial/indigenous-rights
 *
 * SUMMARY:
 *   This constraint story models one specific reading of the Treaty of
 *   Waitangi: the Crown Sovereignty Reading, instantiated through English
 *   Article I. Under this reading, the Crown Parliament received complete,
 *   plenary sovereignty over Aotearoa New Zealand, extinguishing any prior
 *   Māori authority claims. This is the reading that has dominated New
 *   Zealand law from 1840 through much of the 20th century and remains
 *   embedded in current constitutional practice, though now contested by
 *   partnership and rangatiratanga readings. The constraint structure is
 *   Tangled Rope: it coordinates unified governance (genuine coordination
 *   function that solved real post-settlement fragmentation problems) while
 *   simultaneously extracting Māori sovereignty and subordinating Māori
 *   interests to parliamentary will (asymmetric extraction that harms
 *   identifiable victims). The ongoing need for active enforcement (police,
 *   courts, administrative machinery) sustains subordination against
 *   persistent resistance from Māori communities and their advocates.
 *
 * KEY AGENTS:
 *   - Crown Parliament: institutional agenda-setter, holds plenary power, collects decision authority over all matters touching Māori interests
 *   - Settler legal establishment: institutional beneficiaries, judges/lawyers/civil servants operate within framework of Crown sovereignty, income and legitimacy depend on framework stability
 *   - Māori iwi: organized payers, subordinated to parliamentary will on lands, resources, tapu, and governance; constrained exit options (litigation uses same Crown frame; political advocacy requires settler electoral participation)
 *   - Māori landowners: powerless payers, identity-locked to specific lands, face unilateral parliamentary alienation laws and resource licensing; trapped exit
 *   - Māori rights advocates: excluded from the reading's own logic, must argue outside it via litigation, petition, and political organizing
 *   - Courts: analytical observers mediating between readings without formal supremacy override; increasingly expanding what 'sovereignty' permits
 *   - International human-rights bodies: external analytical observers reporting on whether the reading meets postcolonial norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.89).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of the Treaty of Waitangi (English Article I)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/post-colonial/indigenous-rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '76b1034b-5d0d-4490-851f-3e5a6bf1f297').
narrative_ontology:cs_kernel_codification('76b1034b-5d0d-4490-851f-3e5a6bf1f297', fixed_text).
narrative_ontology:cs_authority_grounding('76b1034b-5d0d-4490-851f-3e5a6bf1f297', extraction).
narrative_ontology:cs_interpretation_layer_present('76b1034b-5d0d-4490-851f-3e5a6bf1f297').
narrative_ontology:cs_reading_relation('76b1034b-5d0d-4490-851f-3e5a6bf1f297', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('76b1034b-5d0d-4490-851f-3e5a6bf1f297', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('76b1034b-5d0d-4490-851f-3e5a6bf1f297', foundational, crown_plenary_authority_1840).
narrative_ontology:cs_axiom_status(crown_plenary_authority_1840, holdable).
narrative_ontology:cs_axiom_grounding('76b1034b-5d0d-4490-851f-3e5a6bf1f297', crown_plenary_authority_1840, conventional).
narrative_ontology:cs_axiom('76b1034b-5d0d-4490-851f-3e5a6bf1f297', foundational, maori_sovereignty_extinguished_by_cession).
narrative_ontology:cs_axiom_status(maori_sovereignty_extinguished_by_cession, holdable).
narrative_ontology:cs_axiom_grounding('76b1034b-5d0d-4490-851f-3e5a6bf1f297', maori_sovereignty_extinguished_by_cession, empirically_contingent).
narrative_ontology:cs_reference_frame('76b1034b-5d0d-4490-851f-3e5a6bf1f297', crown_plenary_sovereignty).
narrative_ontology:cs_drift_state('76b1034b-5d0d-4490-851f-3e5a6bf1f297', contemporary_postcolonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76b1034b-5d0d-4490-851f-3e5a6bf1f297', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_legal_establishment).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Westminster Parliament exercises plenary legislative power over Aotearoa New Zealand, claiming full sovereignty under the English Article I reading. Parliament enacts land law, resource allocation, and governance without requiring Māori consent. Justifies this reading as the foundation of stable rule of law and unified governance. The institutional machinery of Parliament — judiciary, civil service, police — operates under this framework.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Judges, lawyers, civil servants, and property developers operate within a legal framework where Parliament's plenary power is foundational. Land titles, contract law, and administrative authority all derive from parliamentary supremacy. Their professional legitimacy and income depend on this framework remaining unchallenged. They benefit from predictable, unilateral rule-setting.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_legal_establishment, beneficiary,
    institutional, generational, mobile, national).

% Iwi (tribal groups) are subordinated to parliamentary will on matters touching their interests: land alienation, resource allocation, cultural governance, and representation. They cannot unilaterally protect taonga (treasured resources and persons), lands, or tapu (sacred protocols) when Parliament legislates contrary. Exit options include litigation (which uses the same Crown legal framework), political advocacy (which requires mobilizing within a settler-dominated electoral system), or economic exit (constrained by prior land dispossession). Their structural position is as subjects receiving parliamentary grace, not as treaty partners or co-sovereigns.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi, payer,
    organized, generational, constrained, national).

% Individual Māori landowners face parliamentary legislation that can alter their property rights, voting rights, and access to resources without negotiation. Historically this took the form of Crown land purchase legislation, resource extraction laws (fisheries, forestry), and alienation of commons. Contemporary forms include compulsory acquisition powers, infrastructure zoning, and unilateral resource licensing to non-Māori extractors. They cannot migrate — their identity and mana (prestige/authority) are tied to specific lands and waters. They lack the capital and legal resources to mount sustained legal challenges.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_landowners, payer,
    powerless, biographical, trapped, local).

% Advocates within Māori communities and settler allies who argue for rangatiratanga or partnership readings are excluded from the crown_sovereignty_reading framework itself — the reading defines their claims as inconsistent with the settled law. They must work through petition, litigation within the inherited frame (asking courts to reinterpret the Treaty), academic argument, and political organizing to contest the reading. They are not at the table where the reading is adjudicated.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_rights_advocates, excluded,
    moderate, biographical, constrained, national).

% Courts (especially the Court of Appeal and Supreme Court, and the Waitangi Tribunal when empowered) examine Treaty texts and history to decide which reading governs specific disputes. They operate within a constitutional tradition where Parliament is sovereign, but they can expand or constrain what that sovereignty means in practice. Their decisions mediate between the Crown sovereignty reading and rival readings without formally overturning parliamentary supremacy.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, treaty_interpretation_courts, observer,
    institutional, generational, analytical, national).

% International human-rights bodies (UN committees on indigenous rights), comparative constitutional scholars, and other postcolonial jurisdictions observe the Treaty framework and verdict on whether it meets international norms of indigenous self-determination and treaty good faith.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes unified legal authority, property title system, and legislative framework for mixed settler-Māori population. Solves fragmentation of governance after 1840 settlement. Early coordination function: getting from multiple parallel authorities to one supreme court, one property registry, one parliament. Later coordination function attenuates.
% TRANSFER_FUNCTION: Transfers sovereign authority and control over land/resources from Māori iwi and hapū to the Crown Parliament. Māori bear the extraction (lost authority, subordination to parliamentary will, loss of control over taonga and lands). Settler legal establishment and Crown Parliament receive the benefit (legitimate monopoly on lawmaking, unilateral resource allocation, income/career advancement from administering Crown authority).
% ABSENT_VOICES: Rangatiratanga advocates and partnership advocates are excluded from the Crown sovereignty reading itself — the reading defines their claims as inconsistent with settled law. They can petition, litigate in courts, and organize politically, but they are not seated within the reading's framework as valid interlocutors. International human-rights bodies are external observers, not seated parties.
% DISAPPEARANCE_RATIONALE: If this reading were replaced by rangatiratanga or partnership readings, the entire constitutional structure of New Zealand would be reorganized: Parliament would lose unilateral authority over Māori domains, Māori veto rights and consent requirements would apply to resource/land decisions, electoral representation would change, and property titles derived under plenary Crown power would face renegotiation. The state apparatus would restructure to accommodate shared sovereignty.
% FOUNDING_PROBLEM: Post-1840 settlement created governance fragmentation: settler law and Māori authority ran in parallel, creating property disputes, contract uncertainties, and potential for violent conflict over resources. The founding problem was establishing a unified legal order so that settler investment and governance could proceed with predictable rules.
% FOUNDING_PROBLEM_CORROBORATION: By 1900, unified property titles, courts, and legislation were fully established — the fragmentation problem is objectively solved. The Crown asserts the founding problem remains live (governance would collapse without Parliament's unilateral authority — a counterfactual claim). Māori rights advocates attest the founding problem is solved and what persists is an extraction mechanism: the coordination function is complete, but the constraint persists to collect rents (control over resources, subordination of Māori interests). International human-rights bodies document the founding problem was solved decades ago and the constraint's persistence is now a violation of postcolonial norms. The corroboration outside Crown-beneficiary parties is clear: the founding problem is solved; the constraint persists as extraction.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.89 at interval end because the constraint transfers control over every major resource and governance decision from Māori to the Crown Parliament; Māori face no veto or negotiation requirement. The measurement series shows steady extraction accumulation (0.72→0.89) as post-WWII and post-1975 resource legislation (Fisheries Act, Resource Management Act, compulsory land acquisition) intensified unilateral Crown authority over what were traditionally Māori domains. Suppression is 0.78 and rising (0.64→0.78) because active enforcement machinery — police, courts, land registration, administrative licensing — is required to suppress alternative authorities and keep Māori communities subordinated to parliamentary will; without this enforcement, Māori institutions would assert parallel authority over lands and taonga. Theater ratio is high and rising (0.35→0.62) because an increasing share of Crown justification for specific acts (fisheries consultation, resource consent processes) is performative: the reading permits parliamentary override of any consultation outcome, so consultation increasingly functions as legitimacy theater rather than genuine power-sharing. The coercion grid shows suppression and stakes-inflation higher at class and organizational levels (Māori iwi and national Māori movements face higher suppression) and lower at individual and structural levels, modeling the Crown's targeting of collective Māori authority while maintaining the appearance of individualized property and civil rights.
 *
 * PERSPECTIVAL GAP:
 *   The Crown Parliament and settler legal establishment perceive this constraint as genuine coordination: unified law, predictable rights, stable governance. From their seat, the constraint's persistence depends on its legitimacy as coordination. The Māori iwi and landowners perceive it as enforced extraction: sovereignty transferred without consent, resources taken unilaterally, resistance suppressed through police and courts. From their seat, the constraint persists because enforcement machinery is active and exit is trapped. The engine computes these divergences from the structural data: institutional power asymmetry, beneficiary/victim declarations, and exit options differentiate the seats. The crown_sovereignty_reading itself asserts that Māori subordination is the natural and correct consequence of the reading, but the measurement series (rising theater and extraction, high suppression) suggests increasing strain — the reading's coordination story is decaying and enforcement is intensifying to sustain it.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown Parliament: d ≈ 0.1 (full beneficiary). Collects sovereign authority, has arbitrage-grade exit (can reinterpret the Treaty, rewrite law, negotiate with other sovereigns), institutional power. Beneficiary role, low suppression experience. Settler legal establishment: d ≈ 0.15 (beneficiary). Income and legitimacy from Crown sovereignty framework, mobile exit (can relocate, reorient to new legal systems), institutional affiliation. Māori iwi: d ≈ 0.88 (high target). Subordinated to parliamentary will, constrained exit (cannot migrate from lands or identity), organized power (more than individual but less than institutional). Victim role. Māori landowners: d ≈ 0.95 (maximum target). Trapped exit (identity-locked to specific places), powerless individually, highest suppression experience, victim role. The asymmetry in d values is large and structural, not observational — the constraint generates different effective extractions for different seats because the seats occupy fundamentally different positions: one set holds the authority machinery, the other is subject to it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits strong mandatrophy symptoms: the founding problem (post-1840 governance fragmentation) is largely solved by 1900 and fully solved by 1920s — unified land law, property titles, and legislative authority are now stable and predictable. Yet the constraint persists and is actively defended long after the problem it was built to solve has been resolved. The theater_ratio rising to 0.62 models the shift from genuine coordination (early period: settling title, establishing courts, creating legislative institutions) to performative maintenance (later period: consultation processes that change nothing, cultural recognition that lacks power, governance forums without veto). The founding_problem_status is 'contested' because the Crown asserts the problem of jurisdictional fragmentation remains live (and hence the solution is still necessary), while rights advocates argue the problem is solved and what remains is an extraction mechanism dressed in the language of coordination. The measurement trajectory (extraction rising, theater rising, suppression rising, resistance rising) models a constraint whose functional justification has atrophied while its extractive and coercive machinery has intensified — a classic Piton transition in progress. However, the constraint is not yet fully Piton because the legal establishment genuinely believes in the coordination story (even though it no longer solves the founding problem), so active rearticulation of the coordination function continues. Call it late-stage Tangled Rope, approaching Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_disparities_between_versions,
    'Do the English and Māori texts of the Treaty describe genuinely different sovereignty allocations, or are they notional equivalents that differ only in translation?',
    'Linguistic and historical analysis comparing: (1) the English text''s cession language (''ceded to the Crown all the rights and powers of Sovereignty''); (2) the Māori text''s kāwanatanga (governorship) language restricted to ''kāwanatanga'' for the Crown and explicit retention of ''tino rangatiratanga'' in Article II. If the terms are equivalent, the readings are interpretive variants; if they describe different authorities, they are structurally distinct constraints.',
    'If the texts differ substantively, the rangatiratanga reading has a textual foundation the Crown sovereignty reading lacks (the reading would need to argue the Māori text was a binding translation error or subordinate to the English version). If equivalent, the readings become disputes over legislative intent and historical practice rather than textual claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_disparities_between_versions, empirical, 'Whether Treaty textual disparities instantiate genuinely different legal claims or are notional translation variants.').

omega_variable(
    intent_of_1840_crown,
    'Did the Crown intend to acquire unilateral sovereignty in 1840, or did it intend to establish a co-sovereigns arrangement constrained by later good-faith obligations?',
    'Historical evidence from Crown records, negotiation notes, instructions to the Crown negotiator (Hobson), and early Crown legal memoranda stating what the Crown believed it had acquired. Māori oral testimony from rangatira (chiefs) and descendants about what chiefs understood themselves to be agreeing to.',
    'If Crown intent was genuinely plenary sovereignty, the Crown sovereignty reading reflects the founding bargain. If Crown intent was co-sovereignty or consultative partnership, the rangatiratanga or partnership readings more accurately reflect the deal struck, and the crown_sovereignty_reading is ex-post-facto unilateral reinterpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_of_1840_crown, empirical, 'Whether the Crown intended to acquire unilateral or shared sovereignty in the 1840 transaction.').

omega_variable(
    codification_of_authority_as_law,
    'Is this constraint''s persistence grounded in genuine legal doctrine (the Crown sovereignty reading is the correct reading of a binding text) or in institutional power (Parliament can assert whatever reading it prefers and enforce compliance through state machinery)?',
    'Test the doctrine''s grip: (1) if courts begin to systematically reinterpret the Treaty under rangatiratanga or partnership framings and Parliament accepts those reinterpretations, the doctrine is yielding to reinterpretation. (2) If Parliament overrides court reinterpretations by statute, the doctrine is subordinate to institutional power.',
    'If doctrine dominates, the constraint is genuinely Tangled Rope (coordination + extraction both depend on the reading''s legitimacy). If institutional power dominates, the constraint is approaching Snare (extraction persists because enforcement machinery wins, not because the reading is believed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_of_authority_as_law, conceptual, 'Whether the reading''s persistence depends on genuine doctrinal legitimacy or primarily on institutional power to enforce compliance.').

omega_variable(
    identity_lock_mechanisms_for_maori,
    'Why do Māori communities remain subordinated within the Crown sovereignty reading despite having exit options (political organizing, litigation, secession movements)? Is the constraint maintained through structural economic dependency, identity-fusion with contested lands, or internalized subordination?',
    'Trace the post-exit trajectory of Māori who leave the constraint''s jurisdiction: do they exit to other nations, establish parallel authority structures, or remain psychologically bound? Examine whether Māori political movements frame demands within the Crown sovereignty frame (reforming Parliament) or against it (asserting pre-Treaty authority). If Māori activists reframe their own identity through anti-colonial lenses, identity lock is eroding.',
    'If identity lock is strong, exit is functionally trapped (identity-locked classification), and suppression is higher. If identity lock is weakening and Māori are increasingly willing to assert parallel authority outside the Crown frame, the constraint''s suppression requirement is rising (theater_ratio rising models this shift).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanisms_for_maori, conceptual, 'Whether Māori subordination is maintained primarily through structural economic dependency, identity-fusion with lands, or internalized psychological subordination, and whether these mechanisms are eroding.').

omega_variable(
    international_norms_pressure,
    'Does international human-rights law (UN Declaration on the Rights of Indigenous Peoples, ILO Convention 169) create structural pressure on this reading to reinterpret toward rangatiratanga or partnership readings?',
    'Monitor: (1) international bodies'' verdicts on whether New Zealand''s constitutional framework meets indigenous-rights standards; (2) whether the Crown or courts cite international norms to justify reinterpretation; (3) whether Māori advocates leverage international norms in domestic litigation.',
    'If international pressure is sustained and New Zealand is attentive to global human-rights standing, the Crown sovereignty reading faces reputational and legal pressure to shift toward partnership or rangatiratanga. This would influence the terminal state (T2) of the constraint''s evolution — a shift toward Piton (inertial maintenance despite external pressure) or toward contested synthesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_norms_pressure, conceptual, 'Whether international human-rights norms create structural pressure on this reading toward reinterpretation.').

omega_variable(
    mandatrophy_confirmed_or_functional_shift,
    'Is the theater_ratio rising trajectory (0.35→0.62) evidence that the coordination function is atrophying (mandatrophy confirmed), or is it evidence that the constraint''s function has shifted from founding-era governance-establishment to ongoing Māori-subordination management (still functional, but different)?',
    'Distinguish by examining what the enforcement machinery is actually doing over the measurement interval. If early enforcement (1840s–1920s) is primarily establishing courts and land titles (genuine coordination), and later enforcement (1960s–present) is primarily suppressing Māori authority assertions (extraction maintenance), the function has shifted. If all periods are primarily suppressing alternatives, the constraint was always Snare dressed as Tangled Rope.',
    'If the function has shifted from coordination to subordination maintenance, mandatrophy is confirmed and the constraint is approaching Piton. If the constraint was always primarily extraction and the coordination story was always cover, the classification should be Snare, and the rising theater models intensifying performance of a failed cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_confirmed_or_functional_shift, conceptual, 'Whether the rising theater_ratio indicates true mandatrophy (coordination function decayed) or functional shift into pure subordination-maintenance, and whether the constraint was always Snare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(wait_grid_01, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(class), 0, 0.71).
narrative_ontology:measurement(wait_grid_02, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(class), 35, 0.82).
narrative_ontology:measurement(wait_grid_03, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(individual), 0, 0.64).
narrative_ontology:measurement(wait_grid_04, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(individual), 35, 0.74).
narrative_ontology:measurement(wait_grid_05, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(wait_grid_06, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(organizational), 35, 0.79).
narrative_ontology:measurement(wait_grid_07, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(wait_grid_08, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(structural), 35, 0.81).
narrative_ontology:measurement(wait_grid_09, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(wait_grid_10, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(class), 35, 0.79).
narrative_ontology:measurement(wait_grid_11, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(individual), 0, 0.42).
narrative_ontology:measurement(wait_grid_12, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(individual), 35, 0.54).
narrative_ontology:measurement(wait_grid_13, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(organizational), 0, 0.64).
narrative_ontology:measurement(wait_grid_14, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(organizational), 35, 0.78).
narrative_ontology:measurement(wait_grid_15, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(wait_grid_16, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(structural), 35, 0.62).
narrative_ontology:measurement(wait_grid_17, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(class), 0, 0.64).
narrative_ontology:measurement(wait_grid_18, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(class), 35, 0.76).
narrative_ontology:measurement(wait_grid_19, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(individual), 0, 0.57).
narrative_ontology:measurement(wait_grid_20, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(individual), 35, 0.69).
narrative_ontology:measurement(wait_grid_21, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(wait_grid_22, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(organizational), 35, 0.68).
narrative_ontology:measurement(wait_grid_23, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(structural), 0, 0.61).
narrative_ontology:measurement(wait_grid_24, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(structural), 35, 0.71).
narrative_ontology:measurement(wait_grid_25, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(wait_grid_26, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(class), 35, 0.8).
narrative_ontology:measurement(wait_grid_27, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(individual), 0, 0.58).
narrative_ontology:measurement(wait_grid_28, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(individual), 35, 0.71).
narrative_ontology:measurement(wait_grid_29, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(wait_grid_30, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(organizational), 35, 0.76).
narrative_ontology:measurement(wait_grid_31, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(structural), 0, 0.65).
narrative_ontology:measurement(wait_grid_32, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(structural), 35, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_alienation_laws).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_fisheries_regulation).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, nz_electoral_representation).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi instantiates three structurally distinct constraints via three different readings of a single kernel text. The Crown Sovereignty Reading (this constraint) models the Crown-Parliament-supremacy interpretation; the Partnership Reading models an ongoing consultation obligation; the Rangatiratanga Reading models reserved Māori authority. These are not variants of one constraint — they are three constraints, each with its own ε, beneficiary/victim structure, and stakeholder configuration. The same 1840 text produces different constraint topologies depending on which reading is instantiated. The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
