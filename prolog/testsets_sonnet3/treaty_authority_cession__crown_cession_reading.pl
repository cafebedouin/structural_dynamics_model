% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Treaty of Waitangi — English-Text Full Sovereignty Cession Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story instantiates the Crown-cession reading of the Treaty of
 *   Waitangi kernel: the English text of the Treaty is treated as legally
 *   controlling, 'kawanatanga' is read as equivalent to full sovereignty, and
 *   the Treaty is treated as having completed a legal cession of authority to
 *   the Crown. This reading grounds the historical and, in significant
 *   respects, ongoing legal architecture of Crown sovereignty, land
 *   alienation, and legislative supremacy in New Zealand. It is authored here
 *   as a single, ε-invariant constraint: extraction is measured as this
 *   reading's own courts, legislatures, and land regimes actually operated,
 *   not averaged against the Maori-text or extraction-focused readings, which
 *   are separate constraints (rangatiratanga_retention_reading,
 *   retrospective_snare_exposure) linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - crown_government: agenda_setter (institutional/arbitrage) — establishes and enforces the cession reading through courts, statute, and land commissions
 *   - settler_land_purchasers: beneficiary (organized/mobile) — acquire land title dependent on the Crown's exclusive pre-emption right
 *   - colonial_legislature: beneficiary/agenda_setter (institutional/arbitrage) — legislates on the premise of plenary sovereignty
 *   - signatory_hapu_and_iwi: payer (organized/trapped) — bore the immediate and continuing consequences of the cession reading's legal operation
 *   - subsequent_generations_of_maori_landholders: payer (powerless/trapped) — inherit compounding land loss traced to this reading
 *   - constitutional_historians: observer (analytical) — document the textual divergence and doctrinal history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.81).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.86).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi — English-Text Full Sovereignty Cession Reading").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '5ad4334f-1598-470d-805b-f19cd1b3b68c').
narrative_ontology:cs_kernel_codification('5ad4334f-1598-470d-805b-f19cd1b3b68c', fixed_text).
narrative_ontology:cs_authority_grounding('5ad4334f-1598-470d-805b-f19cd1b3b68c', extraction).
narrative_ontology:cs_interpretation_layer_present('5ad4334f-1598-470d-805b-f19cd1b3b68c').
narrative_ontology:cs_reading_relation('5ad4334f-1598-470d-805b-f19cd1b3b68c', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('5ad4334f-1598-470d-805b-f19cd1b3b68c', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('5ad4334f-1598-470d-805b-f19cd1b3b68c', foundational, english_instrument_textual_primacy).
narrative_ontology:cs_axiom_status(english_instrument_textual_primacy, holdable).
narrative_ontology:cs_axiom_grounding('5ad4334f-1598-470d-805b-f19cd1b3b68c', english_instrument_textual_primacy, conventional).
narrative_ontology:cs_axiom('5ad4334f-1598-470d-805b-f19cd1b3b68c', foundational, kawanatanga_denotes_full_sovereignty_transfer).
narrative_ontology:cs_axiom_status(kawanatanga_denotes_full_sovereignty_transfer, holdable).
narrative_ontology:cs_axiom_grounding('5ad4334f-1598-470d-805b-f19cd1b3b68c', kawanatanga_denotes_full_sovereignty_transfer, empirically_contingent).
narrative_ontology:cs_axiom('5ad4334f-1598-470d-805b-f19cd1b3b68c', secondary, crown_preemption_land_alienation_legitimate).
narrative_ontology:cs_axiom_status(crown_preemption_land_alienation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('5ad4334f-1598-470d-805b-f19cd1b3b68c', crown_preemption_land_alienation_legitimate, conventional).
narrative_ontology:cs_reference_frame('5ad4334f-1598-470d-805b-f19cd1b3b68c', english_text_full_sovereignty_cession).
narrative_ontology:cs_drift_state('5ad4334f-1598-470d-805b-f19cd1b3b68c', post_waitangi_tribunal_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5ad4334f-1598-470d-805b-f19cd1b3b68c', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_land_purchasers).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, colonial_legislature).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, signatory_hapu_and_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, subsequent_generations_of_maori_landholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts, promulgates, and enforces the English text as the legally controlling instrument. Establishes courts, land commissions, and legislatures that operate on the premise that kawanatanga transferred full sovereignty. Administers land alienation regimes and legislative supremacy built on this reading; can revise or entrench the reading through statute and judicial doctrine.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Acquire land titles that depend on the Crown's exclusive pre-emption right, itself grounded in the claim that sovereignty (not merely governance) passed to the Crown at signing. Benefit from a legal order that treats subsequent land transactions as legitimate transfers rather than contested alienations.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_land_purchasers, beneficiary,
    organized, generational, mobile, national).

% Legislates as though it holds plenary authority over all persons and land within the territory, an authority that only exists under this reading if kawanatanga equalled full sovereignty. Passes land confiscation and native title extinguishment statutes premised on this cession.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, colonial_legislature, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, colonial_legislature, agenda_setter).

% Signed the Maori-text document at Waitangi and elsewhere, understanding kawanatanga as a grant of governance while retaining tino rangatiratanga over their lands, resources, and customary authority. Under this Crown reading, that retained authority is treated as legally extinguished or subordinate; their subsequent objections, petitions, and armed resistance were met with courts and legislatures operating on the cession premise, with no legal exit from the reading once entrenched.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, signatory_hapu_and_iwi, payer,
    organized, generational, trapped, national).

% Inherit a legal landscape shaped by confiscations, forced sales, and native title extinguishment that trace their legitimacy back to this cession reading. Bear compounding land loss and loss of customary governance across generations, with legal remedy channels (courts, tribunals) themselves operating within institutions premised on the same reading they would need to challenge.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, subsequent_generations_of_maori_landholders, payer,
    powerless, civilizational, trapped, national).

% Examine the divergence between the Maori and English texts, the translation choices made by missionary translators, and the subsequent doctrinal history of courts and legislatures. Document how this reading became legally operative and what it displaced.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The English-text reading provided a single, legally administrable sovereignty claim that let the Crown establish courts, currency, land registries, and a unified legislature over the territory rather than negotiating governance piecemeal with hundreds of independent hapu — a genuine coordination problem for a colonial state attempting uniform administration.
% TRANSFER_FUNCTION: Moves land title, legislative authority, and customary governance capacity from signatory hapu and iwi (and their descendants) to the Crown and, through it, to settler purchasers and the colonial legislature — converting what the Maori text describes as a governance grant into a full sovereignty and land-alienation regime.
% ABSENT_VOICES: The rangatiratanga-retention reading's proponents — the chiefs who signed the Maori text and their descendants arguing for the narrower governance-only meaning of kawanatanga — were not treated as parties whose textual understanding controlled; English courts and legislatures adjudicated the meaning of a document those chiefs never read in English.
% DISAPPEARANCE_RATIONALE: If the English-text-controls doctrine were abandoned in favor of the Maori text and its narrower kawanatanga meaning, the legal basis for the Crown's assumed plenary sovereignty, for the pre-emption land purchase regime, and for numerous confiscation and extinguishment statutes would collapse — land title chains, legislative supremacy claims, and existing property distributions would all require re-examination.
% FOUNDING_PROBLEM: The Crown needed a legally cognizable basis for asserting governmental authority over British subjects and settlers already present in New Zealand, and for regulating land transactions with Maori in a way enforceable in British and colonial courts.
% FOUNDING_PROBLEM_CORROBORATION: The Crown's own 19th and 20th century courts and legislatures attest the founding problem was administrative necessity, resolved by this reading. The Waitangi Tribunal — a Crown-established but independently operating body — has repeatedly found, based on comparative textual and historical analysis, that the Maori text does not support a full-sovereignty cession, corroborating from outside the beneficiary set that the founding problem as stated by the Crown reading rests on a translation the signing chiefs did not agree to.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because the reading's operation converted a governance grant (by the Maori text's own terms, per the sibling reading) into full sovereignty, land pre-emption, and legislative supremacy — a substantial transfer of authority and land from signatory hapu and iwi outward. Suppression is authored higher still (0.86 story-level; the temporal series shows a mid-19th-century peak near 0.85 during active confiscation and land-wars enforcement, receding somewhat by the late 20th century as Waitangi Tribunal processes opened partial redress channels, though the reading itself remains the operative legal premise for existing title). Theater ratio rises over the interval (0.15 to 0.42) as land-wars-era direct enforcement gives way to more procedural, doctrinal, and administrative maintenance of the same underlying cession premise. Accessibility collapse is authored high (0.72) reflecting how thoroughly alternative legal premises were foreclosed once colonial courts entrenched this reading; resistance is authored high (0.75) reflecting sustained hapu and iwi legal and political challenge across the full interval.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown/legislature seat, this reading is the coordination solution to a genuine 19th-century administrative problem (uniform governance over a mixed settler-Maori population) and the resulting legal order is legitimate on its own terms. From the signatory hapu and iwi seat, the same structure is an extraction mechanism operating under a translation those signing the Maori text never agreed to. The engine computing tangled_rope from the beneficiary/victim/enforcement structure captures exactly this: a real coordination function (unified colonial administration) riding alongside asymmetric extraction (land and authority transfer) that required — and still requires — active enforcement (land registries, legislative supremacy doctrine, court precedent) to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and colonial legislature sit at the beneficiary/agenda-setter end: they authored, administer, and can in principle revise the reading, and they capture governance authority and land revenue through it. Settler land purchasers are secondary beneficiaries who rely on title chains the reading legitimizes but do not control the reading itself. Signatory hapu and iwi and their descendants sit at the target end: trapped exit options, since the very legal system that would adjudicate an exit operates on the premise being contested, and civilizational time horizon reflecting multi-generational land loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a legally cognizable basis for Crown administration) plausibly was live in 1840; but the reading's continued operation as the premise for present-day land title and legislative authority, long after the immediate administrative-coordination problem could have been resolved through the narrower governance-only meaning, is exactly the mandatrophy signal: the mandate (full sovereignty cession) has outlived any coordination necessity, and persists because Crown, legislature, and downstream titleholders continue to benefit from it, not because the original problem still demands this specific resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translation_equivalence_kawanatanga_sovereignty,
    'Does ''kawanatanga'' in the Maori text carry a meaning equivalent to full sovereignty as asserted by the English text, or a narrower governance-delegation meaning?',
    'Comparative linguistic and historical analysis of 1840s Maori usage of kawanatanga versus mana/rangatiratanga, missionary translation records, and contemporaneous chiefly statements at signing locations.',
    'If kawanatanga cannot bear the full-sovereignty meaning this reading assigns it, the entire legal architecture built on this reading (land pre-emption, legislative supremacy, confiscation statutes) loses its textual foundation and the constraint''s claimed coordination function is substantially undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_equivalence_kawanatanga_sovereignty, conceptual, 'Whether the Crown-cession reading''s core textual premise is linguistically sustainable.').

omega_variable(
    which_text_controls,
    'Should the English text or the Maori text control interpretation of the Treaty, given that most signatories signed the Maori-language version?',
    'Application of contra proferentem and consent-based interpretive doctrine (as advanced by the sibling rangatiratanga_retention_reading) versus the doctrine of state-authored-instrument primacy this reading relies on.',
    'Choice of controlling text is the single largest determinant of which of the sibling readings is legally operative; this reading depends entirely on English-text primacy holding as a matter of law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(which_text_controls, conceptual, 'The foundational interpretive-authority question separating this reading from its siblings.').

omega_variable(
    cession_as_natural_versus_constructed_sovereignty_claim,
    'Is the Crown''s sovereignty claim, once asserted, best understood as a natural/settled fact of subsequent constitutional order (a mountain-like premise no longer contestable), or as a constructed and contestable extraction that happens to have identifiable beneficiaries?',
    'Waitangi Tribunal findings, comparative constitutional practice in other settler-colonial contexts, and ongoing litigation testing whether sovereignty claims grounded in disputed cession remain revisable.',
    'If treated as settled/natural, this reading''s operation would be harder to challenge through ordinary legal or political process; if treated as a constructed and contested extraction, redress and structural revision remain live legal possibilities, as the Waitangi Tribunal process presupposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cession_as_natural_versus_constructed_sovereignty_claim, conceptual, 'Whether the cession claim underlying this reading functions as settled constitutional bedrock or as a contestable extractive construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__crown_cession_reading, theater_ratio, 1860, 0.2).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__crown_cession_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement(trea_tr_t1950, treaty_authority_cession__crown_cession_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(trea_tr_t1990, treaty_authority_cession__crown_cession_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__crown_cession_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1860, 0.68).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1900, 0.79).
narrative_ontology:measurement(trea_be_t1950, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1950, 0.82).
narrative_ontology:measurement(trea_be_t1990, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.5).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1860, 0.75).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(trea_su_t1950, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1950, 0.83).
narrative_ontology:measurement(trea_su_t1990, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__crown_cession_reading, 0.1).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language 'Treaty of Waitangi authority question' per the epsilon-invariance principle. crown_cession_reading (this story) authors epsilon=0.81 for the English-text-controls, full-sovereignty-cession arrangement as it actually operated. rangatiratanga_retention_reading authors a structurally distinct constraint (Maori-text controls, kawanatanga limited to governance, ongoing-consent partnership) with its own epsilon and victim/beneficiary structure. retrospective_snare_exposure treats the textual divergence itself as the extraction mechanism, exposing mistranslation as the operative device. All three share the kernel treaty_authority_cession but are linked, not merged, via network edges; no single epsilon value is shared across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
