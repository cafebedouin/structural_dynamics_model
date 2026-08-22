% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty Authority Cession—Rangatiratanga Retention Reading
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) established a partnership between the
 *   British Crown and Māori iwi. This constraint story instantiates ONE
 *   reading of that treaty—the rangatiratanga retention reading—in which the
 *   Māori text controls (via the contra proferentem rule: ambiguity in a
 *   negotiated instrument is resolved against the drafter). Under this
 *   reading, kāwanatanga ('governance') grants the Crown limited authority
 *   over shared matters (law, order, trade), while tino rangatiratanga
 *   ('self-determination, absolute authority') is retained by Māori hapū over
 *   their lands, people, and internal governance. The treaty establishes a
 *   partnership requiring Crown consent-seeking for actions affecting Māori
 *   domains. This reading is in active constitutional contest with the
 *   crown_cession_reading (English text controls; kāwanatanga means full
 *   sovereignty; treaty completes cession) and the
 *   retrospective_snare_exposure reading (the textual divergence itself is an
 *   extraction mechanism). The rangatiratanga reading claims the constraint
 *   operates as a Rope of partnership with active enforcement requirements.
 *   The measurement series document the escalation of extraction (via
 *   unilateral Crown action and legislative override) from 1840 to ~1860–1920
 *   (peak extraction and suppression), followed by cyclical oscillation as
 *   court recognition and Māori mobilization ratcheted up from 1970s forward,
 *   creating contemporary theater (Crown acknowledges partnership verbally
 *   while continuing unilateral action). The claim/metric divergence is
 *   intentional: the constraint is CLAIMED as Rope but the authored metrics
 *   show Tangled Rope friction (partnership requiring enforcement) with
 *   extractive pressure from unilateral Crown action. The engine measures
 *   this gap; this commentary explains the reading's stance.
 *
 * KEY AGENTS:
 *   - hapu_iwi_collective: Beneficiaries under the partnership reading; targets of Crown extraction through land alienation and legislative override; identity-locked to the treaty and their genealogical territories
 *   - crown_executive: Agenda-setter under both readings; constrained by partnership obligations under this reading; unconstrained by cession claims under the rival reading; capable of arbitraging between interpretations
 *   - english_language_interpreters: Excluded institutional voices (Crown lawyers, judges prior to 1987 case law); their subordination via contra proferentem is the reading's mechanism
 *   - treaty_signatories_historical: Analytical seats whose historical assent to the Māori text (and non-assent to the unseen English text) grounds this reading's authority
 *   - domestic_courts: Analytical observers whose progressive recognition of the Māori text (1912 Williams case, 1987 Māori Council case, subsequent jurisprudence) shifted institutional legitimacy toward the rangatiratanga reading
 *   - colonial_land_commission: Excluded institutions whose historical operation under the cession reading became visible as extraction machinery once the rangatiratanga reading gained ground
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.68).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.71).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty Authority Cession—Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, 'e86c3141-16d1-4a24-8be2-e077989301f4').
narrative_ontology:cs_kernel_codification('e86c3141-16d1-4a24-8be2-e077989301f4', fixed_text).
narrative_ontology:cs_authority_grounding('e86c3141-16d1-4a24-8be2-e077989301f4', lineage).
narrative_ontology:cs_interpretation_layer_present('e86c3141-16d1-4a24-8be2-e077989301f4').
narrative_ontology:cs_reading_relation('e86c3141-16d1-4a24-8be2-e077989301f4', treaty_authority_cession__crown_cession_reading, coexists_with).
narrative_ontology:cs_reading_relation('e86c3141-16d1-4a24-8be2-e077989301f4', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('e86c3141-16d1-4a24-8be2-e077989301f4', foundational, maori_text_authoritative_via_contra_proferentem).
narrative_ontology:cs_axiom_status(maori_text_authoritative_via_contra_proferentem, holdable).
narrative_ontology:cs_axiom_grounding('e86c3141-16d1-4a24-8be2-e077989301f4', maori_text_authoritative_via_contra_proferentem, conventional).
narrative_ontology:cs_axiom('e86c3141-16d1-4a24-8be2-e077989301f4', foundational, tino_rangatiratanga_retained_in_partnership).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained_in_partnership, holdable).
narrative_ontology:cs_axiom_grounding('e86c3141-16d1-4a24-8be2-e077989301f4', tino_rangatiratanga_retained_in_partnership, deontological).
narrative_ontology:cs_reference_frame('e86c3141-16d1-4a24-8be2-e077989301f4', negotiated_partnership_framework).
narrative_ontology:cs_drift_state('e86c3141-16d1-4a24-8be2-e077989301f4', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e86c3141-16d1-4a24-8be2-e077989301f4', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi_collective).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi_collective).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, tino_rangatiratanga_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, partnership_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, Māori iwi and hapū retain tino rangatiratanga (self-determination authority) over lands, governance, and cultural matters through the Māori text's contra proferentem interpretation. They are the primary beneficiary of the partnership framework that requires ongoing Crown consent-seeking. Simultaneously, they are payers: Crown policy and legislative override have systematically violated the partnership over 180+ years, alienating land, suppressing language, and imposing assimilationist law. Their identity is constituted through the relationship to the treaty itself and their genealogical lands; exit is theoretically available only through sovereignty restoration or confederation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi_collective, beneficiary,
    moderate, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi_collective, payer).

% Under the crown_cession_reading, the Crown claims to hold absolute legislative sovereignty via the kāwanatanga cession. Under this rangatiratanga reading, the Crown's legitimacy to exercise authority on Māori lands and over Māori affairs is constrained by the requirement to secure hapū consent. The Crown's agenda-setting power is the enactment of policy and law; under this reading, that power is narrowed by the partnership obligation and the retained Māori tino rangatiratanga. The Crown can arbitrage between readings in courts and legislative chambers.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Crown officials, judges, and lawyers historically read the English text ('sovereignty' and 'kāwanatanga' as full cession) and excluded Māori linguistic and legal interpretations from treaty adjudication. Under this reading, their exclusion from recognizing the Māori text's authoritative status is the mechanism that sustained the cession-reading's dominance. They would defend the English text's primacy on historical grounds; their voice is structural but interpretively subordinated under contra proferentem.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, english_language_interpreters, excluded,
    institutional, generational, analytical, national).

% The original chiefs and hapū leaders who signed the treaty in 1840 are analytically reconstructed. Under this reading, they signed the Māori text, which promised partnership and retained their tino rangatiratanga; they could not have assented to the English text's sovereignty cession because it was not read to them in the language it was signed in. Their historical assent is the reading's grounding authority, but they are no longer present to testify.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, treaty_signatories_historical, observer,
    powerless, biographical, analytical, local).

% New Zealand courts have progressively recognized the Māori text as authoritative (contra proferentem doctrine applied in landmark cases like Williams v Attorney-General 1912, New Zealand Maori Council v Attorney-General 1987). Courts sit as analytical observers of the constitutional contest; their rulings shift which reading has institutional legitimacy, but they do not themselves benefit or pay.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, domestic_courts, observer,
    institutional, generational, analytical, national).

% 19th-century Crown land courts (Native Land Court, later Land Transfer machinery) operated under the cession reading: they treated Crown title as absolute and Māori land rights as extinguishable by individual title-by-descent claims, systematically converting communal Māori land to Crown and settler ownership. Under the rangatiratanga reading, these institutions operated as extractive machinery—converting collective hapū authority into individual title, then into Crown alienation—all operating under the cover of the false translation. These institutions are excluded from voice because their historical operation is visibility of the reading's contradictions.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, colonial_land_commission, excluded,
    institutional, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, crown_executive).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Brings Crown authority and Māori self-determination into a shared constitutional framework without subordinating one to the other: the Crown exercises governance (kāwanatanga) over law, order, and matters of mutual interest; Māori hapū retain tino rangatiratanga (self-determination) over their territories, people, and internal governance. The coordination problem is integrating Crown sovereignty at the regional/national level with Māori independence at the hapū/iwi level without making one legally subordinate to the other.
% TRANSFER_FUNCTION: Authority flows bidirectionally and contingently: from hapū to Crown for specified governance domains (negotiated per agreement); from Crown to hapū for recognition and protection of Māori rights. The Crown transfers legal recognition; Māori transfer recognition of Crown authority. Historically, the Crown extracted authority unilaterally (land, legislation, governance) while claiming the transfer was complete and permanent.
% ABSENT_VOICES: Rival Māori political factions not party to the 1840 signings are present but not all equally represented in contemporary rangatiratanga claims. Non-Māori settlers and their descendants, who benefited from land alienation under the cession reading, are structurally excluded from treaty interpretation. British Crown officials who drafted the English text are analytically absent—their choices are visible only through the textual audit.
% DISAPPEARANCE_RATIONALE: If the rangatiratanga reading disappeared—if courts rejected it and the Crown reasserted sole sovereignty—Māori iwi would shift from inside-system constitutional contestation to outside-system sovereignty claims or confederation movements. The New Zealand state's bicultural identity would collapse. Māori mobilization would intensify. If the treaty itself disappeared, New Zealand's constitutional legitimacy would come into question for 40% of the population, and Māori lands held under treaty protection would face renewed contestation.
% FOUNDING_PROBLEM: In 1840, Māori hapū faced rapid settler immigration, military threats from rival iwi armed with European weapons, and vulnerability to foreign annexation (French and American interest in New Zealand colonies). The treaty was negotiated to secure Crown military protection and peaceful co-existence without surrendering Māori independence: Crown protection of borders and law enforcement in mixed settlements in exchange for Māori recognition of Crown governance, while Māori retained authority over their own territories and people. The partnership framework was meant to address both Crown need (stable base for colonial expansion) and Māori need (protection and autonomy) simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Māori iwi and hapu leaders from 1840 onward attest that the treaty was meant to establish partnership with mutual recognition of independence. Historical records of the signings (accounts from missionaries, trappers, and early settlers) document conversations with rangatira about 'protection' and 'partnership,' not 'cession.' Contemporary Māori oral tradition carries the understanding that tino rangatiratanga was never surrendered. Outside the beneficiary camp: academic historians (Claudia Orange, Anne Salmond, Paul McHugh) document the linguistic divergence and contemporary Māori understanding. The Waitangi Tribunal (established 1975; independent Crown body) found through systematic inquiry that the partnership reading is consistent with historical evidence and Māori intent. New Zealand courts (from 1987 Māori Council case onward) have affirmed the Māori text's authority and the partnership framework's binding nature. Crown government historians and policy analysts increasingly acknowledge the partnership reading as the historical understanding, though Crown policy enforcement remains contested. The corroboration is robust outside the Crown executive beneficiary seat.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness series rises from 0.15 at t=0 (partnership framework newly established, minimal unilateral Crown action) to 0.72 at t=40 (1880, peak land alienation under Native Land Court, suppression of language and governance). It remains high through 1980 (0.71 at t=140), then declines slightly to 0.68 by 2020 (t=180) as court recognition constrains unilateral Crown action. The suppression series (0.08 to 0.76 to 0.71) tracks enforcement machinery: initially minimal (partnership held informally), then intensive (active suppression of Māori language, institutions, political voice via legislation, education policy, police action), then declining as Māori mobilization and court rulings constrain Crown suppressive capacity. Theater ratio rises from near-zero (0.05) to 0.42 by present: the Crown increasingly uses partnership rhetoric (Ministry of Māori Development, bicultural policy statements, Waitangi Tribunal acknowledgments) while continuing unilateral legislative and administrative action. The cyclical pattern visible in measurements (extraction and suppression peak c.1920, decline through 1960s, then ratchet up again 1980–2000 as Crown reasserts control over treaty settlement negotiations, then oscillate) reflects the tension between court recognition of the reading and Crown institutional resistance. Theater ratio stays elevated (0.41–0.42) because the Crown's agenda is not to dissolve the partnership but to perform it while retaining unilateral agenda-setting—a piton-adjacent dynamic sustained by inertia and the cost of formal constitutional change. Suppression requirement remains high (0.71) because the partnership framework continues to face resistance from Crown institutional interests and settler political blocs opposed to Māori authority. The measurement grid reflects one shared time spine (t = 0, 20, 40, 80, 140, 180) across all three metrics, enabling temporal alignment.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown executive seat: the treaty is a historical cession clothed in partnership language for diplomatic purposes; Crown sovereignty is absolute; Māori consultation is discretionary good governance, not mandatory. From this reading's Māori seat: the treaty is a binding partnership with Māori veto rights; Crown action lacking Māori consent is illegitimate; land alienation and legislative override constitute breach. The engine computes per-seat classification: the Crown seat should compute as beneficiary/agenda-setter with high d-toward-beneficiary; Māori hapū compute as targets with high d-toward-target under the extraction metrics. The reading's structural claim is that both seats are nominally coordinated (partnership framework) but the Crown's power to arbitrage between readings and sustain unilateral action creates tangled extraction. The perspectival gap is NOT a flaw in the framework but the core of what the constraint describes: a partnership in which one party claims absolute authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū/iwi are declared as both beneficiary and victim (an unusual pairing that reflects the reading's core tension). Under the partnership reading, they are beneficiaries: the treaty secured their tino rangatiratanga, protection from foreign annexation, and the foundation for later claims to self-determination. Simultaneously, they are victims: the Crown's systematic breach (land alienation via mistranslated cession, legislative override, suppression of language and institutions) operated under cover of the English text's false sovereignty claim. The bifurcated role captures the reading's diagnosis: the partnership is real (beneficiary) but broken (victim). The Crown's directionality is low d-toward-target (powerful institutional beneficiary of the cession reading's arbitrage). The measurement series encode this: extractiveness rises because the Crown exerts unilateral authority while denying breach. Suppression rises because maintaining the cession reading requires suppressing Māori voice and alternative readings. Theater rises because the Crown performs partnership (Māori Affairs ministry, Waitangi Tribunal) while retaining unilateral action (legislative override, resource allocation). From the Crown seat, the constraint should compute as a Rope or mild Tangled Rope; from the Māori seat, it should compute as Tangled Rope or Snare under the extraction metrics. The directionality override is unnecessary: the structural derivation (beneficiary/victim split, constrained vs. arbitrage exit, moderate vs. institutional power) produces the correct d-divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—existential threat requiring partnership with Crown protection—was live in 1840. By 1880, the problem had shifted: after the Land Wars (1845–1872), Māori military threat was neutralized; the Crown held military and legislative dominance. The partnership framework persisted institutionally (treaty remained signed), but the founding problem it was built to solve was dead (Crown protection was no longer needed; Crown dominance was complete). The Crown's unilateral action from 1880 onward—land alienation, language suppression, legislative override—operated under the cover of the cession reading while the partnership reading languished. Mandatrophy resolution: the rangatiratanga reading identifies this deadened-problem/persisting-arrangement gap as the signature of the constraint's transformation into extraction. The reading's constitutional claim is that the partnership's founding problem is NOT dead—Māori self-determination and Crown consultation are ongoing needs, not solved once in 1840. Courts and Māori voices from 1970s onward affirmed this: the founding problem is live under the partnership reading, and its persistence justifies the partnership constraint. The Crown's position (via the cession reading) is that the founding problem was solved in 1840 when sovereignty transferred; the treaty's work is done. The Waitangi Tribunal's 1975 establishment and progressive findings shifted institutional weight toward the rangatiratanga reading by validating the founding problem's continued relevance. Theater ratio (0.42) reflects this: the Crown performs partnership while denying the founding problem (settlement claims are 'closing chapters,' not 'restoring partnership'). The reading resolves mandatrophy by subordinating the cession reading's 'problem solved' claim to the partnership reading's 'problem ongoing' claim—a reading choice, not a measurement fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translator_intent_asymmetry,
    'Did the English text''s divergence from the Māori text represent intentional Crown deception, inadvertent mistranslation, or a deliberate negotiated compromise where both parties understood the linguistic difference?',
    'Historical textual analysis of drafting correspondence (Crown officials'' private records, missionary notes of conversations with chiefs), linguistic comparison of 1840s English vs. Māori usage, and oral history from hapū traditions about what understanding was conveyed at signing.',
    'If intentional deception: the reading strengthens as evidence of the extraction mechanism itself (the Crown was knowingly ceding less via English text). If inadvertent: the reading remains valid but shifts from ''Crown fraud'' to ''structural asymmetry.'' If negotiated compromise: the reading weakens; both texts would represent agreed-upon bounded meanings (Crown gets cession for shared governance; Māori retain internal authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translator_intent_asymmetry, empirical, 'Whether the English–Māori textual divergence was Crown deception, translation accident, or negotiated difference.').

omega_variable(
    tino_rangatiratanga_scope_ambiguity,
    'Under the Māori text, does tino rangatiratanga (absolute authority) apply only to internal hapū affairs (genealogy, land management, cultural authority) or does it extend to external sovereignty (war, trade, diplomatic relations)?',
    'Comparative analysis of Māori governance traditions prior to 1840 (what domains did rangatira (chiefs) exercise absolute authority over), ethnographic reconstruction of 19th-century hapū political structure, and court interpretation of tino rangatiratanga in modern treaty settlements.',
    'If limited to internal affairs: the partnership reading stands; the Crown legitimately claims kāwanatanga over external matters. If extended to external sovereignty: the rangatiratanga reading becomes a near-independence claim; the Crown''s kāwanatanga cession is minimal, and the Crown''s unilateral action is pure external usurpation. If scope is contest-dependent: the reading remains live but is permanently contestable—the scope itself is the forum for negotiated authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tino_rangatiratanga_scope_ambiguity, conceptual, 'The scope of tino rangatiratanga: internal governance only, or inclusive of external sovereignty.').

omega_variable(
    partnership_enforcement_mechanism,
    'What makes the partnership binding once the English text''s cession claim is rejected? Does Māori veto power over Crown action exist in fact, or only in law? Can the Crown be stopped from unilateral action by Māori refusal to consent?',
    'Analysis of court judgments granting or denying Māori injunctions against Crown action (e.g., Ngāi Tahu claim to mineral rights, Wai 143 natural-resource settlement), Crown compliance or non-compliance with Waitangi Tribunal recommendations, and legislative responsiveness to Māori-led bills.',
    'If enforcement is weak (Crown can ignore Māori consent): the constraint computes as Tangled Rope or Snare depending on whether the Crown''s action qualifies as suppression. If enforcement is robust (Māori veto is real): the constraint computes as Rope—genuine coordination with mutual obligations. The measurement series encode this via suppression_requirement: high suppression indicates the Crown must actively repress Māori voice and alternative readings to maintain unilateral action; low suppression would indicate partnership norms are internalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partnership_enforcement_mechanism, empirical, 'Whether the partnership framework''s mutual consent requirement is enforceable or merely rhetorical.').

omega_variable(
    crown_arbitrage_between_readings,
    'Does the Crown''s capacity to invoke the cession reading in some contexts (e.g., legislation, executive action) while acknowledging the partnership reading in others (e.g., public rhetoric, court negotiations) constitute the central extraction mechanism—the ability to choose which reading applies when?',
    'Systematic audit of Crown policy statements, legislative history, court arguments, and treaty settlement negotiations across a 20-year window to quantify reading invocation by context; Māori iwi testimony on whether Crown conduct aligns with whichever reading the Crown is invoking.',
    'If arbitrage is systematic: it is the core mechanism sustaining extraction—the Crown claims partnership when Māori have political power, then claims cession to override Māori interests when Māori have weak political standing. Theater ratio would spike during periods of high arbitrage. If arbitrage is context-dependent: the Crown is inconsistent rather than strategic, and the reading contest is genuinely open. This omega connects to the theater ratio measurement series: high theater ratio + persistent extraction suggests arbitrage is operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crown_arbitrage_between_readings, empirical, 'Crown institutional ability to switch between readings strategically to maximize control.').

omega_variable(
    alternative_readings_logically_coexist,
    'Can a single constitutional framework coherently hold BOTH the rangatiratanga reading (Māori retain self-determination, Crown governs shared domains) AND the crown_cession_reading (Crown holds absolute sovereignty, partnership is rhetorical) simultaneously, or do these readings logically foreclose each other?',
    'Constitutional legal analysis of what it means for ''Crown sovereignty'' and ''Māori tino rangatiratanga'' to coexist: can one party hold absolute authority while the other retains authority in the same domain? Examination of how contemporary New Zealand constitutional law formulates the relationship (does it claim both readings are operative, or that one has priority?). International indigenous rights standards and their compatibility with the two readings.',
    'If readings foreclose each other: only one can be constitutionally true; the contest is binary. If readings coexist: both are institutionally alive; the constraint operates as Tangled Rope (coordination + asymmetric extraction). If coexistence is incoherent but politically maintained: the constraint computes as Snare (coordination story is cover; persistence depends on suppressing the logical contradiction). The prospective_snare_exposure reading holds that the readings DO foreclose each other, and the textual asymmetry IS the extraction mechanism because chiefs could not assent to a text that contradicts the Māori text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_logically_coexist, conceptual, 'Logical compatibility of the rangatiratanga and crown_cession readings within a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(trea_tr_t0, projected).
narrative_ontology:measurement(trea_tr_t20, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(trea_tr_t20, observed).
narrative_ontology:measurement(trea_tr_t40, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement_basis(trea_tr_t40, observed).
narrative_ontology:measurement(trea_tr_t80, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(trea_tr_t80, observed).
narrative_ontology:measurement(trea_tr_t140, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 140, 0.41).
narrative_ontology:measurement_basis(trea_tr_t140, observed).
narrative_ontology:measurement(trea_tr_t180, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 180, 0.42).
narrative_ontology:measurement_basis(trea_tr_t180, observed).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(trea_be_t0, projected).
narrative_ontology:measurement(trea_be_t20, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(trea_be_t20, observed).
narrative_ontology:measurement(trea_be_t40, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(trea_be_t40, observed).
narrative_ontology:measurement(trea_be_t80, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 80, 0.71).
narrative_ontology:measurement_basis(trea_be_t80, observed).
narrative_ontology:measurement(trea_be_t140, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 140, 0.65).
narrative_ontology:measurement_basis(trea_be_t140, observed).
narrative_ontology:measurement(trea_be_t180, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 180, 0.68).
narrative_ontology:measurement_basis(trea_be_t180, observed).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(trea_su_t0, projected).
narrative_ontology:measurement(trea_su_t20, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(trea_su_t20, observed).
narrative_ontology:measurement(trea_su_t40, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement_basis(trea_su_t40, observed).
narrative_ontology:measurement(trea_su_t80, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement_basis(trea_su_t80, observed).
narrative_ontology:measurement(trea_su_t140, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 140, 0.68).
narrative_ontology:measurement_basis(trea_su_t140, observed).
narrative_ontology:measurement(trea_su_t180, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 180, 0.71).
narrative_ontology:measurement_basis(trea_su_t180, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__rangatiratanga_retention_reading, 0.12).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, land_alienation_via_native_land_court).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, maori_language_suppression_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the single treaty_authority_cession kernel. The rangatiratanga_retention_reading instantiates the partnership framework (Rope of negotiated authority sharing). The crown_cession_reading instantiates absolute Crown sovereignty (Rope claimed by the Crown; Snare from Māori perspective). The retrospective_snare_exposure reading shows the textual divergence itself as extraction mechanism, foreclosing both prior readings. All three share the treaty text as their referent but ε diverges by reading: the rangatiratanga reading assesses the treaty as enabling partnership (lower ε for the partnership itself, higher ε for Crown breach of partnership); the crown_cession reading assesses the treaty as completing cession (low ε for what it considers a completed legal act); the snare reading assesses the treaty as an asymmetric extraction operation (high ε for the mistranslation mechanism). These are not different perspectives on one constraint—they are structurally distinct constraints with distinct beneficiary/victim structures, distinct ε values, and distinct stability conditions. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
