% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__retrospective_snare_exposure, []).

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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty of Waitangi Textual Divergence as Retrospective Extraction Mechanism
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story instantiates the retrospective_snare_exposure reading of the
 *   treaty_authority_cession kernel: the claim that the divergence between
 *   the Māori text (kāwanatanga/tino rangatiratanga) and the English text
 *   (full sovereignty cession) is not a mere interpretive dispute to be
 *   resolved by choosing which text controls, but is itself the extraction
 *   mechanism. On this reading, the mechanism was covert at the moment of
 *   signing — chiefs assented to a document whose terms differed materially
 *   from the document the Crown subsequently enforced — and became visible
 *   only retrospectively, through nineteenth-and-twentieth-century land
 *   alienation, Native Land Court individualization of title, and legislative
 *   override, all conducted under the cover of an English-text sovereignty
 *   claim Māori signatories never assented to. This is structurally distinct
 *   from the crown_cession_reading (which holds the English text simply
 *   controls and treats the outcome as legitimate cession) and from the
 *   rangatiratanga_retention_reading (which holds the Māori text controls via
 *   contra proferentem and treats the ongoing arrangement as an unfulfilled
 *   partnership obligation). This reading takes no position on which text
 *   SHOULD control as a matter of interpretive method; its claim is that the
 *   existence of two divergent texts, one used to obtain signature and the
 *   other used to justify extraction, is itself the operative extractive
 *   structure, and that this structure was invisible to the payer class at
 *   the point of signature by design or negligence, becoming visible only
 *   through retrospective historical and legal analysis (Ross 1972 onward).
 *
 * KEY AGENTS:
 *   - crown_land_purchasing_apparatus: institutional beneficiary that drafted, relied upon, and enforced the English-text sovereignty claim while land purchase commissions operated
 *   - maori_treaty_signatories: primary historical victims — organized power (as rangatira) but trapped exit — who signed a text materially different from the one subsequently enforced against them
 *   - maori_descendant_communities and hapu_landholding_collectives: downstream victims bearing compounding, generational and civilizational-scale costs of the original extraction
 *   - colonial_and_successor_courts: agenda-setting institutional actor whose adjudicative choice to treat the English text as sole authority operationalized the extraction
 *   - waitangi_tribunal: analytical/observer seat created by the Crown itself, which documents but historically could not reverse the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.79).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi Textual Divergence as Retrospective Extraction Mechanism").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '9272c8cf-981c-4efa-8477-1b02ab7997f1').
narrative_ontology:cs_kernel_codification('9272c8cf-981c-4efa-8477-1b02ab7997f1', fixed_text).
narrative_ontology:cs_authority_grounding('9272c8cf-981c-4efa-8477-1b02ab7997f1', extraction).
narrative_ontology:cs_interpretation_layer_present('9272c8cf-981c-4efa-8477-1b02ab7997f1').
narrative_ontology:cs_reading_relation('9272c8cf-981c-4efa-8477-1b02ab7997f1', treaty_authority_cession__crown_cession_reading, influences).
narrative_ontology:cs_reading_relation('9272c8cf-981c-4efa-8477-1b02ab7997f1', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('9272c8cf-981c-4efa-8477-1b02ab7997f1', foundational, divergent_text_signature_cannot_constitute_assent_to_unshown_terms).
narrative_ontology:cs_axiom_status(divergent_text_signature_cannot_constitute_assent_to_unshown_terms, holdable).
narrative_ontology:cs_axiom_grounding('9272c8cf-981c-4efa-8477-1b02ab7997f1', divergent_text_signature_cannot_constitute_assent_to_unshown_terms, deontological).
narrative_ontology:cs_axiom('9272c8cf-981c-4efa-8477-1b02ab7997f1', foundational, extraction_mechanism_identifiable_independent_of_controlling_text_resolution).
narrative_ontology:cs_axiom_status(extraction_mechanism_identifiable_independent_of_controlling_text_resolution, holdable).
narrative_ontology:cs_axiom_grounding('9272c8cf-981c-4efa-8477-1b02ab7997f1', extraction_mechanism_identifiable_independent_of_controlling_text_resolution, conventional).
narrative_ontology:cs_reference_frame('9272c8cf-981c-4efa-8477-1b02ab7997f1', dual_text_signing_event_1840).
narrative_ontology:cs_drift_state('9272c8cf-981c-4efa-8477-1b02ab7997f1', post_ross_1972_historiographic_reassessment, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9272c8cf-981c-4efa-8477-1b02ab7997f1', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_colonial_administration).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_treaty_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, hapu_landholding_collectives).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, crown_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, terra_nullius_adjacent_legal_fiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and circulated the English-text sovereignty cession while the instrument actually signed by most chiefs was the Māori-language text promising kāwanatanga (governance) while guaranteeing tino rangatiratanga (chieftainship/full authority) over lands and taonga. Subsequently relied on the English text to authorize land purchase commissions, native land courts, and legislative override, extracting land title and jurisdictional authority through a document divergence it controlled and did not correct.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter).

% Signed the Māori-language text in 1840 in reliance on assurances (oral, from missionaries and Crown representatives, and textual, via the kāwanatanga/rangatiratanga distinction) that they retained full authority over their lands, resources, and internal governance. Had no access to, and could not have assented to, the English-text claim of unqualified sovereignty cession. Once the English text became the operative legal instrument in Crown and colonial courts, this seat had no mechanism to contest a translation asymmetry it did not create and, at signing, could not have known existed.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_treaty_signatories, payer,
    organized, generational, trapped, national).

% Inherit the compounding consequences of land alienation and jurisdictional subordination that trace directly to the English-text version being treated as authoritative. Their exit options today run through Waitangi Tribunal claims and settlement negotiation — a channel created by the Crown itself and bounded by what the Crown is willing to concede, not a channel that restores the pre-1840 position.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities, payer,
    organized, civilizational, constrained, national).

% Customary landholding units whose collective title was reclassified, individualized, or extinguished by Native Land Court processes and subsequent legislation that presupposed the Crown's English-text sovereignty claim. Had no forum in which to assert that the underlying authority for these processes was never validly obtained under the instrument they actually signed.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, hapu_landholding_collectives, payer,
    moderate, generational, trapped, regional).

% For over a century treated the English text as the sole legally cognizable instrument, foreclosing judicial inquiry into the Māori text's divergent terms. This adjudicative choice is itself part of the extraction mechanism: it converted a translation dispute into a settled legal fact, insulating the Crown's land purchase apparatus from challenge on the actual terms signed.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, colonial_and_successor_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Established in 1975 (retroactive jurisdiction extended in 1985) to hear claims regarding Crown breaches of the Treaty's principles, including the textual divergence. Can investigate and recommend but historically lacked binding authority to reverse land transfers or invalidate legislation grounded in the English text — its findings document the extraction mechanism without being able to unwind it.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, excluded).

% Inherits a constitutional order whose foundational legitimacy is contested precisely because of this divergence; benefits diffusely from land and infrastructure whose title chain runs through the disputed extraction, without individually having authored or profited in a concentrated way from the original mechanism.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, contemporary_new_zealand_public, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine at the point of extraction — the textual divergence was not a coordination mechanism at all but a translation gap the Crown's purchasing apparatus relied upon rather than corrected. Insofar as any coordination function exists, it is retrospective: a shared legal fiction that the English text was always the operative agreement, which lets courts and legislatures proceed without re-litigating founding legitimacy.
% TRANSFER_FUNCTION: Moves land title, natural resource rights, and governmental authority from Māori signatory hapū and their descendants to the Crown and successor settler-colonial institutions, executed through land purchase commissions, the Native Land Court's individualization of title, and legislative override — all authorized by treating the English text's sovereignty claim as valid despite the absence of Māori assent to that specific claim.
% ABSENT_VOICES: The chiefs who signed the Māori text are the paradigmatic absent voice: they could not object to an English-language sovereignty cession they never saw, in a language most did not read, containing a legal claim fundamentally broader than what was orally and textually represented to them. Their descendants remain structurally excluded from the forums (ordinary courts, until very recently) that determined the instrument's legal meaning.
% DISAPPEARANCE_RATIONALE: If the English-text-as-authoritative convention were overturned and the Māori text's more limited kāwanatanga grant were retroactively treated as the operative agreement, the entire chain of land title, statutory authority, and constitutional sovereignty claims resting on it would require re-examination — settlements, land registries, and the constitutional basis of Crown authority over Māori affairs would all be subject to renegotiation.
% FOUNDING_PROBLEM: The stated founding problem (per the Crown's own framing) was establishing orderly governance over a colony experiencing increasing settler in-migration and inter-hapū and settler-Māori conflict, while (per the Māori-text framing) chiefs sought protection from lawless settlers and other colonial powers while retaining authority over their own people and lands.
% FOUNDING_PROBLEM_CORROBORATION: Historians (Ruth Ross's 1972 textual analysis; subsequent Waitangi Tribunal historical reports) and the Tribunal itself — an institution created by the Crown but staffed with independent commissioners and historians outside the land-purchasing apparatus — corroborate that the two texts diverge materially on the sovereignty question and that Māori signatories could not have assented to the English text's terms. This corroboration comes from outside the beneficiary set: professional historians and a quasi-judicial body were not parties who gained from establishing the divergence.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.88) because the mechanism transferred land, resource rights, and governmental authority at a scale and permanence with no meaningful restitution channel available to the payer class at the time of operation. Suppression is authored high (0.79) because the mechanism depended on courts and legislatures actively refusing to recognize the Māori text as legally operative for well over a century — an affirmative institutional choice to suppress the alternative reading, not a passive gap. Theater ratio is authored moderate-high (0.62) because significant apparatus now exists (Tribunal hearings, settlement processes, commemorative acknowledgment) that performs redress without the underlying land and authority transfers being reversed at anything like the scale of the original extraction — the theater ratio is authored as rising over the interval as commemorative and consultative activity expanded while the substantive property/authority reversal lagged far behind. Accessibility collapse is authored moderate-low (0.35), reflecting that the alternative reading (that the Māori text should control) was never actually inaccessible in principle — it was suppressed by institutional refusal to hear it, not because no coherent alternative existed; this is a snare sustained by active institutional suppression of a live alternative, not a mountain where no alternative exists. Resistance is authored high (0.81), reflecting sustained Māori political, legal, and land-occupation resistance across the entire interval.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown_land_purchasing_apparatus and its settler-colonial administrative successors sit at the full-beneficiary end: they authored the divergence's use, controlled which text was cited in law, and captured the land and authority that flowed from that choice. Māori treaty signatories and their hapū sit at the full-target end: organized as a polity (rangatira held real authority) but trapped in exit terms — there was no comparable polity to flee to, and the extraction operated through the very instrument they had signed in good faith. Descendant communities inherit victim status by structural succession even though they did not personally sign, because the land and jurisdictional losses compound across generations. The Waitangi Tribunal and courts are treated as agenda-setting/observer seats rather than beneficiaries or victims themselves — they administer or investigate the mechanism but do not personally collect from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem framing prevents this reading from over-claiming that the entire treaty relationship was extraction from inception. The founding problem — orderly governance amid rising settler conflict, and Māori protection from lawless outsiders — was genuinely live in 1840 on both textual framings; what makes this a snare rather than a scaffold or tangled rope is that the specific mechanism under examination (textual divergence weaponized retrospectively) served no coordination function whatsoever from the Māori signatory's vantage point — it was covert at signing and became an instrument of override only once litigated. This is distinguished from the rangatiratanga_retention_reading's live partnership-obligation framing: that reading holds the coordination function persists and is owed; this reading holds the specific mechanism of textual divergence-as-authority is pure extraction regardless of which text one thinks should ultimately control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_divergence_intent_vs_negligence,
    'Was the divergence between the Māori and English texts a deliberate drafting choice by Crown officials (Hobson, Freeman, Busby) to secure signature under a narrower representation while retaining the broader English claim, or an artifact of rushed translation under time pressure with no deliberate extractive intent?',
    'Archival analysis of drafting correspondence, comparison with contemporaneous colonial office instructions, and historical linguistic analysis of the translation process (building on Ross 1972 and subsequent Tribunal historical reports).',
    'Deliberate intent would strengthen the snare classification (extraction as designed mechanism); pure negligence would leave the extraction structurally intact (Māori still could not assent to terms they were not shown) but shift the moral valence of the beneficiary''s culpability, though not the structural fact of unequal benefit capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_intent_vs_negligence, empirical, 'Whether textual divergence was designed or accidental extraction machinery.').

omega_variable(
    which_text_governs_as_a_separate_question,
    'Is the question of which text should retroactively govern (crown_cession_reading vs. rangatiratanga_retention_reading) separable from the question of whether the divergence itself constituted extraction regardless of resolution?',
    'Conceptual clarification: does resolving the interpretive question (which text controls) dissolve the extraction claim, or does the extraction claim persist under either resolution because the signatories could not have assented to the eventually-enforced claim at the time of signing?',
    'If the extraction claim persists under either textual resolution, this reading is robust to the outcome of the sibling kernel contest; if the extraction claim only holds under the crown_cession_reading being wrong, then this reading is parasitic on rangatiratanga_retention_reading''s success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_text_governs_as_a_separate_question, conceptual, 'Whether the extraction-mechanism claim is independent of or dependent on the sibling interpretive dispute.').

omega_variable(
    restitution_adequacy,
    'Do current Waitangi Tribunal settlements constitute adequate resolution of the extraction (converting the constraint toward scaffold/transitional status), or do they represent a fraction of value extracted that leaves the snare structurally live?',
    'Quantitative comparison of settlement quantum against independent valuations of land and resource value extracted, adjusted for time value and compounding development value.',
    'If settlements are found broadly adequate, the constraint''s classification could shift toward a resolved-but-historical snare or even scaffold-in-transition; if settlements are found to represent a small fraction of extracted value, the snare remains substantially live into the present.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restitution_adequacy, preference, 'Whether existing settlement processes adequately remedy the extraction or merely perform remedy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.2).
narrative_ontology:measurement_basis(trea_tr_t1840, observed).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.3).
narrative_ontology:measurement_basis(trea_tr_t1860, observed).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.4).
narrative_ontology:measurement_basis(trea_tr_t1900, observed).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1940, 0.5).
narrative_ontology:measurement_basis(trea_tr_t1940, observed).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.55).
narrative_ontology:measurement_basis(trea_tr_t1975, observed).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2000, 0.6).
narrative_ontology:measurement_basis(trea_tr_t2000, observed).
narrative_ontology:measurement(trea_tr_t2020, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2020, 0.62).
narrative_ontology:measurement_basis(trea_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement_basis(trea_be_t1840, observed).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.7).
narrative_ontology:measurement_basis(trea_be_t1860, observed).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.85).
narrative_ontology:measurement_basis(trea_be_t1900, observed).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1940, 0.87).
narrative_ontology:measurement_basis(trea_be_t1940, observed).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.82).
narrative_ontology:measurement_basis(trea_be_t1975, observed).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement_basis(trea_be_t2000, observed).
narrative_ontology:measurement(trea_be_t2020, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement_basis(trea_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.65).
narrative_ontology:measurement_basis(trea_su_t1840, observed).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.85).
narrative_ontology:measurement_basis(trea_su_t1860, observed).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.9).
narrative_ontology:measurement_basis(trea_su_t1900, observed).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1940, 0.85).
narrative_ontology:measurement_basis(trea_su_t1940, observed).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement_basis(trea_su_t1975, observed).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement_basis(trea_su_t2000, observed).
narrative_ontology:measurement(trea_su_t2020, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(trea_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).

% DUAL FORMULATION NOTE:
% This constraint is the third member of the treaty_authority_cession kernel family. crown_cession_reading and rangatiratanga_retention_reading both take positions on which text should control as a matter of interpretive method and thereby on whether the treaty's authority claim is currently valid or breached-but-owed. This reading (retrospective_snare_exposure) is analytically prior to both: it identifies the textual-divergence-as-mechanism itself as the extractive structure, independent of which interpretive resolution one adopts, and treats the mechanism's visibility only in retrospect (via 20th-century historical and legal scholarship) as a defining feature distinguishing it from either sibling's live-dispute framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
