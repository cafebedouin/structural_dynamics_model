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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of the Treaty of Waitangi (English Article I, Westminster Supremacy)
 *   domain: constitutional/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This story authors the Crown-sovereignty reading of the Treaty of
 *   Waitangi kernel: the claim, grounded in the English-language Article I
 *   text, that the Treaty effected a complete cession of sovereignty to the
 *   British Crown, establishing unqualified Westminster parliamentary
 *   supremacy over New Zealand. Under this reading, Māori interests are
 *   politically and legally subordinate to Parliament's will; resource and
 *   land allocation proceeds by statute without a structural Māori consent
 *   requirement. This is the reading that, historically, courts and
 *   government applied as operative constitutional fact from 1840 through
 *   most of the twentieth century, and which underwrites Crown radical title
 *   doctrine and the historic non-justiciability of Treaty claims. It is one
 *   of three sibling readings of the same kernel
 *   (waitangi_sovereignty_allocation); the partnership_reading and
 *   rangatiratanga_reading are separate constraint stories with their own ε
 *   values and stakeholder structures, linked here via
 *   network.affects_constraints. Per the ε-invariance principle, this file's
 *   extraction, suppression, and beneficiary/victim structure are authored
 *   solely for THIS reading's own operative claim, not averaged against or
 *   hedged by the sibling readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.81).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.87).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of the Treaty of Waitangi (English Article I, Westminster Supremacy)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '921ca481-335e-4f63-8c84-c6b0d13fd43c').
narrative_ontology:cs_kernel_codification('921ca481-335e-4f63-8c84-c6b0d13fd43c', fixed_text).
narrative_ontology:cs_authority_grounding('921ca481-335e-4f63-8c84-c6b0d13fd43c', extraction).
narrative_ontology:cs_interpretation_layer_present('921ca481-335e-4f63-8c84-c6b0d13fd43c').
narrative_ontology:cs_reading_relation('921ca481-335e-4f63-8c84-c6b0d13fd43c', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_reading_relation('921ca481-335e-4f63-8c84-c6b0d13fd43c', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('921ca481-335e-4f63-8c84-c6b0d13fd43c', foundational, english_text_is_controlling_instrument).
narrative_ontology:cs_axiom_status(english_text_is_controlling_instrument, holdable).
narrative_ontology:cs_axiom_grounding('921ca481-335e-4f63-8c84-c6b0d13fd43c', english_text_is_controlling_instrument, conventional).
narrative_ontology:cs_axiom('921ca481-335e-4f63-8c84-c6b0d13fd43c', foundational, sovereignty_transfer_admits_no_consent_condition).
narrative_ontology:cs_axiom_status(sovereignty_transfer_admits_no_consent_condition, holdable).
narrative_ontology:cs_axiom_grounding('921ca481-335e-4f63-8c84-c6b0d13fd43c', sovereignty_transfer_admits_no_consent_condition, conventional).
narrative_ontology:cs_reference_frame('921ca481-335e-4f63-8c84-c6b0d13fd43c', westminster_parliamentary_supremacy_1840).
narrative_ontology:cs_drift_state('921ca481-335e-4f63-8c84-c6b0d13fd43c', post_treaty_principles_jurisprudence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('921ca481-335e-4f63-8c84-c6b0d13fd43c', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_and_settler_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_landholders).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractive_industry_licensees).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_and_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_claimants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_fisheries_and_resource_holders).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_radical_title_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the English-text reading as the operative constitutional fact: Parliament holds plenary legislative power, courts historically treated Māori claims as non-justiciable, and land and resource allocation proceeds by statute without a Māori consent requirement. Sets the terms of native title extinguishment, land confiscation, and resource licensing, and enforces them through the ordinary machinery of law and, historically, through military and police force.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_and_settler_government, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_and_settler_government, beneficiary).

% Received title to land alienated from Māori under Crown pre-emption and later Crown grant, validated by the doctrine that sovereignty (not merely governance) passed at cession. Their security of title depends on the English-text reading remaining the operative constitutional premise; a shift to a rangatiratanga-based reading would reopen title questions they treat as settled.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_landholders, beneficiary,
    organized, generational, mobile, national).

% Hold Crown-issued licenses for minerals, fisheries quota, and forestry on land and foreshore whose underlying title rests on the Crown's claim to have acquired full sovereignty (and with it radical title to land and resources) in 1840. Their license security is downstream of this reading prevailing over the rangatiratanga reading.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractive_industry_licensees, beneficiary,
    powerful, biographical, mobile, national).

% Signed the Māori-text Treaty ceding kāwanatanga, not sovereignty over their own affairs, yet have been governed for most of the constitutional period as though full sovereignty passed. Bear the confiscations, forced sales, and legislative overrides of customary title that followed from treating Article I's English text as controlling. Exit from the jurisdiction is not available; recourse runs only through the same Crown institutions whose authority is in question.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_and_hapu, payer,
    powerless, civilizational, trapped, national).

% Pursue restitution through Crown-created forums (courts, the Waitangi Tribunal) whose jurisdiction and remedial power are themselves creatures of the parliamentary supremacy this reading establishes. Can litigate and settle but cannot compel a reallocation of underlying sovereign authority; settlements are financial and symbolic, not a return of the constitutional position they claim under the Māori text.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_claimants, payer,
    moderate, generational, constrained, national).

% Customary fishing and resource rights were treated as extinguished or subordinated by statute under the premise that the Crown, having acquired full sovereignty, could allocate and regulate resources without their consent. Quota and settlement mechanisms exist but operate downstream of the sovereignty premise rather than displacing it.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_fisheries_and_resource_holders, payer,
    moderate, generational, constrained, national).

% Interpret the constitutional weight of the two Treaty texts. For most of the period since 1840, courts applied the English-text/parliamentary-supremacy premise directly (e.g., historic non-justiciability doctrine); more recently some have qualified it through the principles jurisprudence without abandoning it. Their rulings both apply and partially soften the constraint.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, constitutional_courts_and_tribunals, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, constitutional_courts_and_tribunals, agenda_setter).

% Analyze the divergence between the English and Māori Treaty texts and the doctrinal history of how the Crown-sovereignty premise became operative despite the ambiguity, drawing comparisons to other settler-colonial cession doctrines.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_and_settler_government).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, stable locus of legislative and judicial authority (Westminster-style parliamentary supremacy) so that one coherent body of law can govern a territory with two very differently positioned populations, avoiding the coordination failure of parallel or contested legal orders.
% TRANSFER_FUNCTION: Moves land, resource control, and governing authority from Māori iwi and hapu to the Crown and, through it, to settler landholders and licensees — justified as the necessary consequence of a sovereignty transfer the Māori-text signatories did not, on the rangatiratanga reading, agree to.
% ABSENT_VOICES: Māori rangatira who signed the Māori-text Treaty are the primary absent voice: the English-text reading that grounds this constraint was drafted in English, not translated with equivalent legal effect into the Māori text they actually signed, and their understanding of what they ceded (kāwanatanga, not sovereignty) is structurally excluded from this reading's operative premise.
% DISAPPEARANCE_RATIONALE: If the Crown-sovereignty reading were displaced as the operative constitutional premise, land title chains resting on Crown radical title would be open to challenge, resource licensing regimes would require renegotiation with iwi and hapu as sovereign or co-sovereign parties, and the entire post-1840 statutory apparatus of native title extinguishment would lose its founding legal warrant — a rearrangement on the scale of a constitutional refounding, not an incremental legal adjustment.
% FOUNDING_PROBLEM: The Crown sought a legally cognizable basis under British and international law of the period to assert exclusive authority over New Zealand territory, pre-empting rival European claims and providing settlers with a stable sovereign framework for land acquisition and governance.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and its courts historically attested the founding problem as solved and settled (sovereignty acquired, governance stabilized). Independent corroboration from outside the benefiting parties is mixed and points the other way on the reading's own terms: the Waitangi Tribunal (a Crown-created but statutorily independent body), international legal scholars examining Treaty/treaty-making doctrine of the 1840 period, and Māori legal scholars have documented that the Māori-text signatories did not textually cede sovereignty, meaning the 'founding problem' this reading claims to have solved was, on its own textual evidence, not the problem actually agreed to by one signatory party.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.81) because, on this reading's own terms, land and resource transfer from Māori to Crown/settler interests proceeded as a direct legal consequence of the sovereignty-cession premise, without a consent mechanism for the party whose Māori-text signature did not contain that premise. Suppression is authored very high (0.87) reflecting that the reading's persistence has depended on active enforcement — from nineteenth-century land confiscation under military force, through non-justiciability doctrine foreclosing judicial challenge, to ongoing statutory schemes that treat sovereignty as settled. The measurement series shows a suppression and extraction spike around 1863 (New Zealand Wars-era confiscations under the Settlements Act) followed by mid-twentieth-century moderation as direct coercion receded, then a late-period uptick reflecting renewed doctrinal and political assertion of parliamentary supremacy against principles-based erosion (e.g. legislative responses reasserting Crown authority over foreshore and seabed in the 2000s). Theater ratio rises across the century as non-justiciability doctrine and later symbolic Treaty commemorations substituted for substantive consent mechanisms — performative acknowledgment increasing even as the underlying sovereignty premise remained unmoved.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown/settler agenda-setter seat, this reading is Rope or at most a defensible constitutional settlement: it solved a genuine coordination problem (one legal order for a mixed territory) and its beneficiaries experience it as legitimate governance, not extraction. From the Māori payer seats, the identical structure is experienced as enforced extraction riding on a coordination story — the 'coordination' was never consented to under the text they signed. The engine's per-seat computation should reproduce exactly this divergence: the same positional data yielding rope-like readings from institutional/beneficiary seats and tangled-rope/snare-like readings from powerless/trapped payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and settler-descended landholders and licensees are structural beneficiaries: their title, licensing, and governing authority derive directly from the sovereignty-cession premise, and they have low d (subsidized by the constraint). Māori iwi, hapu, land claimants, and resource holders are the structural targets: the premise directly authorizes the extinguishment and reallocation of their customary authority and property, and their exit options are trapped or, at best, constrained to Crown-created remedial forums that themselves presuppose the premise being contested — this locks their d near the full-target end regardless of formal legal capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a legally cognizable, exclusive Crown sovereignty basis) is contested as still 'live' by the Crown's own historic doctrine, but is corroborated as never having been the problem the Māori-text signatories agreed to solve, by evidence outside the beneficiary set (the Māori text itself, Waitangi Tribunal findings, and comparative treaty scholarship). This is precisely the mandatrophy signature: an arrangement whose declared founding problem is disputed by parties outside those who benefit from perpetuating the declared solution — treating the 1840 cession as settled forecloses re-examination that the arrangement's own founding text does not support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_divergence_authority,
    'When the English and Māori texts of a treaty diverge on the core question of what was ceded (sovereignty vs. governorship), which text controls the constitutional reading, and by what authority is that choice made?',
    'Comparative international law analysis of treaty interpretation doctrine at time of signing (1840) versus contemporary doctrine; examination of whether unilateral post-hoc textual preference by one party (the Crown, choosing its own English draft) can be treated as dispositive.',
    'If the Māori text is held authoritative (as most contemporary Treaty scholarship holds), the crown_sovereignty_reading''s foundational premise collapses and the constraint this story describes has never had the textual warrant it claims — reclassifying it from a contested-but-live constitutional doctrine toward an acknowledged historical extraction dressed as sovereignty transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_authority, conceptual, 'Whether the English or Māori text is the controlling instrument for what was ceded in 1840.').

omega_variable(
    sovereignty_versus_kawanatanga_translation,
    'Did the Māori concept of kāwanatanga (governorship), used in the Māori-text Article I, carry an equivalent meaning to the English ''sovereignty'' at the time of translation, or was it a deliberately or accidentally narrower transfer?',
    'Historical linguistic and ethnographic analysis of 1840 Māori political vocabulary, missionary translation practices, and contemporaneous rangatira testimony (where recorded) about their understanding of what was signed.',
    'A finding of deliberate narrowing (kāwanatanga as consciously lesser than full sovereignty) strengthens the case that this reading was never the agreed bargain and functions as extraction with a coordination veneer, not genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_versus_kawanatanga_translation, empirical, 'Whether the Māori and English texts encoded the same scope of authority transfer.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the crown_sovereignty_reading logically foreclose the rangatiratanga_reading within a single legal framework, or can both persist as competing doctrinal positions within the same constitutional order (as they have, uneasily, in New Zealand jurisprudence)?',
    'Doctrinal history review: examine whether courts and Parliament have, in practice, held both premises simultaneously (e.g. asserting parliamentary supremacy while also recognizing Treaty principles and partial rangatiratanga in specific statutory contexts), which would indicate coexistence rather than foreclosure.',
    'If coexistence is the operative pattern (as New Zealand''s actual constitutional practice since the 1970s Treaty principles jurisprudence suggests), the reading_relations for this kernel should reflect ongoing tension rather than clean logical exclusion, informing how the sibling constraint stories are linked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether Crown sovereignty and rangatiratanga readings can coexist doctrinally or are mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.2).
narrative_ontology:measurement(wait_tr_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1863, 0.15).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(wait_tr_t1950, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement(wait_be_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1863, 0.88).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1900, 0.82).
narrative_ontology:measurement(wait_be_t1950, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1950, 0.75).
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1975, 0.68).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.5).
narrative_ontology:measurement(wait_su_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1863, 0.95).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(wait_su_t1950, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2024, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the waitangi_sovereignty_allocation kernel. crown_sovereignty_reading (this file) authors high extraction/suppression consistent with unilateral sovereignty assertion and Māori subordination. partnership_reading authors a moderated, partially-coordinative structure premised on good-faith consultation duties recognized in later jurisprudence. rangatiratanga_reading authors the Māori-text premise under which the Crown never acquired authority over Māori-retained lands/resources/taonga, making Crown resource allocation under that reading a much higher-extraction, victim-clearer snare-leaning structure. The three share stakeholders (Crown, iwi/hapu, settlers) but diverge sharply in ε, beneficiary/victim assignment, and claimed_type because they are, per the ε-invariance principle, structurally distinct constraints sharing a contested textual kernel, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
