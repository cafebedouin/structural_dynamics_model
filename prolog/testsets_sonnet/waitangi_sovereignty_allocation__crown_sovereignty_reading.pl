% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Crown Sovereignty Reading of the Treaty of Waitangi (English Article I)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint isolates the Crown-sovereignty reading of the Treaty of
 *   Waitangi: the position, historically operative in colonial and much
 *   subsequent New Zealand law, that the English-language Article I effected
 *   a complete cession of sovereignty from Māori rangatira to the British
 *   Crown, establishing Westminster-style parliamentary supremacy over the
 *   whole territory including Māori. Under this reading, Crown legislative
 *   power over land, resources, and Māori affairs requires no Māori consent
 *   as a matter of sovereign right — Māori interests are protected, if at
 *   all, only by legislative grace or later statutory concession (e.g. the
 *   Treaty of Waitangi Act 1975), never by an entrenched consent requirement.
 *   This is deliberately one reading among three that this kernel supports;
 *   the partnership_reading and rangatiratanga_reading are separate
 *   constraint files with different beneficiary/victim structures and
 *   different epsilon values, linked here via network.affects_constraints.
 *   The historical record shows this reading's practical extraction rising
 *   sharply during the New Zealand Wars and confiscation era (1860s),
 *   moderating through the mid-20th century as direct land seizure slowed,
 *   then intensifying again in political salience (though not necessarily in
 *   raw land transfer) as the reading was invoked defensively against
 *   Waitangi Tribunal findings and co-governance proposals from the 1990s
 *   onward — hence the extractiveness uptick at 2020 despite declining raw
 *   land-transfer volume; the metric tracks structural leverage of the
 *   reading, not only physical land movement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.81).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of the Treaty of Waitangi (English Article I)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'bdabf7f7-79eb-4e73-895d-742c0f1e11b0').
narrative_ontology:cs_kernel_codification('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', fixed_text).
narrative_ontology:cs_authority_grounding('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', extraction).
narrative_ontology:cs_interpretation_layer_present('bdabf7f7-79eb-4e73-895d-742c0f1e11b0').
narrative_ontology:cs_reading_relation('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', foundational, english_text_governs_sovereignty_question).
narrative_ontology:cs_axiom_status(english_text_governs_sovereignty_question, holdable).
narrative_ontology:cs_axiom_grounding('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', english_text_governs_sovereignty_question, conventional).
narrative_ontology:cs_axiom('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', foundational, parliamentary_supremacy_admits_no_consent_veto).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_admits_no_consent_veto, holdable).
narrative_ontology:cs_axiom_grounding('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', parliamentary_supremacy_admits_no_consent_veto, conventional).
narrative_ontology:cs_reference_frame('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', english_text_plenary_cession).
narrative_ontology:cs_drift_state('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', post_waitangi_tribunal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bdabf7f7-79eb-4e73-895d-742c0f1e11b0', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_land_purchasers).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, colonial_administration).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_owners).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_political_representation).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, westminster_parliamentary_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the English-text reading as the operative constitutional fact: Article I is read as a cession of complete sovereignty, from which plenary Westminster-style legislative power over all persons and lands in New Zealand follows without any structural requirement for Māori consent. Legislates land law, native title extinguishment mechanisms, and resource allocation unilaterally. Can revise its own reading through statute or judicial appointment without external check.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Acquire land through Crown-administered purchase and confiscation processes legitimated by the sovereignty cession reading. Their title security depends entirely on the Crown's sovereignty claim being treated as complete and unchallengeable; they benefit from every subsequent land court and legislative act premised on that claim.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_land_purchasers, beneficiary,
    organized, generational, mobile, national).

% Native Land Court, provincial governments, and colonial officials operationalize the sovereignty-cession reading into administrative practice — surveying, titling, and confiscating land under statutes that presuppose plenary Crown authority. Their institutional legitimacy and revenue base are built on the reading holding.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, colonial_administration, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, colonial_administration, agenda_setter).

% Signed the Māori-language text (Te Tiriti), which most hapū understood as ceding kāwanatanga, not full sovereignty. Under this English-text reading, their subsequent objections, petitions, and armed resistance are treated as domestic insurrection against a sovereign already established, rather than as breach-of-treaty grievances between parties. Exit from the constraint requires either armed resistance (crushed at severe cost) or litigation within a legal system whose foundational premise is the very sovereignty claim being contested.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_hapu, payer,
    powerless, civilizational, trapped, national).

% Lose land through confiscation (raupatu) and forced individualization of communal title, both premised on the Crown's unilateral sovereignty being total and requiring no consent for the exercise of legislative power over Māori land. No structural mechanism under this reading requires their agreement to land legislation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_owners, payer,
    powerless, generational, trapped, regional).

% For decades held no meaningful voice in the Westminster-style parliament whose supremacy this reading establishes; later confined to a fixed, disproportionately small number of reserved seats. Would argue for a partnership or shared-sovereignty framework but the plenary-power premise structurally forecloses any veto or co-governance claim from being cognizable as a matter of sovereign right rather than legislative grace.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_political_representation, excluded,
    powerless, generational, constrained, national).

% Examine the divergence between the English and Māori texts, colonial-era Hansard, and subsequent case law (from Wi Parata through to the Treaty of Waitangi Act era and Court of Appeal jurisprudence) to assess which reading the historical and textual record actually supports and how the Crown-sovereignty reading became judicially entrenched despite the translation gap.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_government).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, administratively workable locus of legislative and judicial authority over the territory — one parliament, one law, one court system — avoiding the coordination failure of parallel or contested sovereignties within one jurisdiction.
% TRANSFER_FUNCTION: Moves land, resource control, and political authority from Māori hapū and land owners to the Crown and, through Crown-administered purchase and confiscation, to settler purchasers; moves the power to define the terms of that transfer entirely into Crown hands.
% ABSENT_VOICES: Māori signatories to Te Tiriti (the Māori-language text) are structurally absent from the reading's own foundation: the reading operationalizes the English text alone, treating the Māori text's promise of tino rangatiratanga as either subordinate or void. Their objection — that they never ceded complete sovereignty — is excluded from the reading's own premises by construction.
% DISAPPEARANCE_RATIONALE: If the Crown-sovereignty reading were displaced as the operative constitutional premise, land confiscation statutes, extinguishment of native title without consent, and the plenary-power basis of parliamentary supremacy over Māori affairs would all lose their legal foundation; land restitution, co-governance arrangements, and a consent requirement for legislation affecting Māori interests would become live constitutional questions rather than settled fact.
% FOUNDING_PROBLEM: The Crown sought a legal basis to establish exclusive governmental authority over British settlers and Māori alike, to regularize land purchase, suppress inter-hapū conflict as the Crown perceived it, and to forestall other colonial powers (notably France) from establishing a rival claim to the islands.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and colonial courts (through the 19th and much of the 20th century) attest the sovereignty-cession problem is fully and permanently resolved by the English text. Independent constitutional historians, the Waitangi Tribunal's own findings, and Māori legal scholars — sources outside the beneficiary set — attest that the Māori-language text was the version actually explained and signed by most rangatira, and that the 'complete sovereignty cession' reading is a retrospective construction resting on the version fewer signatories read.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.81) is authored high because the reading's practical effect — legitimating confiscation, non-consensual land legislation, and the subordination of Māori political voice to a parliament Māori had negligible presence in — is a direct, large-scale transfer of land and authority with no genuine reciprocal benefit built into the reading's own premises (unlike a rope, where participants are net beneficiaries). Suppression (0.78) reflects that the reading's persistence depended on active coercion: military campaigns in the 1860s (spiking suppression_requirement to 0.92), subsequent land court mechanisms designed to individualize and alienate communal title, and continued judicial deference to parliamentary supremacy that forecloses a consent-based challenge. Theater ratio (0.42) is moderate: some of the apparatus (native land courts, treaty settlement processes) performs a legitimacy function that has partially decoupled from the reading's founding coercive function, particularly post-1975 as statutory Treaty principles were layered on without disturbing the underlying sovereignty-cession premise. Accessibility collapse (0.62) and resistance (0.74) are both substantial and roughly balanced — this is not a settled natural fact (that would show near-total accessibility collapse with negligible resistance, as in a mountain) but a contested, actively defended constitutional claim that has met sustained resistance from Māori litigation, protest, and the Waitangi Tribunal process for over 150 years and has never fully closed off the rival readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown government and colonial administration sit at the beneficiary end: they author, administer, and revise the sovereignty-cession reading and collect the resulting authority and, via land alienation, its economic proceeds. Settler land purchasers benefit derivatively — their title security is entirely parasitic on the reading holding. Māori hapū, land owners, and political representation sit at the target end: trapped or severely constrained exit (armed resistance was militarily crushed; litigation must proceed within a legal system whose foundational premise is the claim being contested), civilizational time horizon (the harm compounds across generations of land loss and political exclusion), and no structural mechanism for consent. The directionality here is not symmetric in the way a genuine coordination mechanism would produce — it is a one-way transfer dressed, at the level of the reading's own self-justification, as the establishment of orderly government.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem attributed to this reading — establishing a single locus of governmental authority to prevent inter-colonial rivalry and regularize settlement — was, on the corroborating record, real in 1840 but was never a problem requiring COMPLETE, non-consensual sovereignty cession to solve; the partnership_reading and rangatiratanga_reading show that lesser allocations of authority (kāwanatanga over settlers, active protection of Māori interests) could have addressed the same coordination problem without the extractive premise. The Crown-sovereignty reading's status is contested precisely because the reading's own beneficiaries (Crown, colonial administration) maintain the founding problem is closed and fully resolved by the English text, while independent historians and the Waitangi Tribunal's findings attest the reading was a retrospective, self-serving construction. This is the mandatrophy pattern: a claim about founding necessity (single sovereign authority) used to justify a scope of extraction (complete, unilateral, non-consensual power over Māori land and governance) far exceeding what the founding problem required — and the classification as tangled_rope rather than pure snare recognizes that SOME genuine coordination function (a single legal order) is real, while the asymmetric extraction riding on top of it is the object requiring justification and is what this reading, taken alone, fails to justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translation_gap_determinacy,
    'Which text — the English Article I (''cede to Her Majesty the Queen of England absolutely and without reservation all the rights and powers of Sovereignty'') or the Māori Article I (using ''kawanatanga'', governorship) — reflects what the signing rangatira actually understood and agreed to?',
    'Documentary and linguistic-historical analysis of contemporaneous missionary translation practice, explanations given at signing (recorded in colonial and Māori oral accounts), and comparison with how ''sovereignty'' concepts were rendered elsewhere in 1830s-40s Māori-language documents.',
    'If the Māori text is taken as controlling (as most signatories encountered it), the Crown-sovereignty reading''s foundational premise — complete cession — collapses, and the rangatiratanga_reading becomes the textually dominant claim; this reading would then be classified as resting on a construction rather than the treaty''s actual terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_gap_determinacy, empirical, 'Whether the English or Māori text reflects the actual agreement reached at signing.').

omega_variable(
    natural_vs_constructed_sovereignty_claim,
    'Is ''complete Crown sovereignty without consent requirement'' a natural, self-evident feature of statehood formation in 1840 international law, or a constructed reading serving identifiable Crown and settler beneficiaries?',
    'Comparative analysis against other 19th-century treaty-based sovereignty transfers (e.g., other Pacific and North American indigenous treaties) to assess whether ''complete cession without consent mechanism'' was the standard legal expectation or an outlier construction.',
    'If constructed rather than a natural incident of treaty-making, the reading is more clearly a tangled_rope (coordination cover for extraction) rather than any variant of settled constitutional fact; this bears on whether the reading should be treated as contestable policy or entrenched law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_sovereignty_claim, conceptual, 'Whether the plenary-sovereignty premise is a natural incident of 1840 treaty law or a self-serving construction.').

omega_variable(
    judicial_entrenchment_mechanism,
    'How did the Crown-sovereignty reading become judicially entrenched (e.g. Wi Parata v Bishop of Wellington 1877, describing the Treaty as a ''simple nullity'') despite the textual ambiguity, and is that entrenchment itself now eroding through later jurisprudence (e.g. Waitangi Tribunal-influenced case law, Ngati Apa)?',
    'Doctrinal case-law tracing from Wi Parata through to contemporary Court of Appeal and Supreme Court treatment of Treaty principles.',
    'A clear erosion trajectory would support classifying the reading''s current suppression_requirement trajectory as declining rather than the modestly rising value authored for 2020, refining the temporal profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_entrenchment_mechanism, empirical, 'Trajectory of judicial entrenchment and erosion of the sovereignty-cession premise over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(wait_tr_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1863, 0.2).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(wait_tr_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1940, 0.38).
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(wait_tr_t2020, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement(wait_be_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1863, 0.72).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1900, 0.78).
narrative_ontology:measurement(wait_be_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1940, 0.74).
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1975, 0.68).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(wait_be_t2020, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2020, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.5).
narrative_ontology:measurement(wait_su_t1863, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1863, 0.92).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(wait_su_t1940, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(wait_su_t2020, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% Three constraint files decompose the single natural-language label 'Treaty of Waitangi sovereignty allocation': crown_sovereignty_reading (this file — plenary Crown power, no consent requirement, highest extractiveness), partnership_reading (ongoing good-faith consultation obligation, moderate extractiveness, tangled_rope with a more balanced beneficiary/victim structure), and rangatiratanga_reading (Māori retention of full authority over lands/resources/taonga, lowest Crown extractiveness, closest to a genuine rope from the Māori signatory perspective). Each has a distinct epsilon and distinct victim/beneficiary sets because each reading allocates sovereignty differently as a matter of the reading's own internal logic — they are not the same constraint viewed from different angles but three structurally distinct claims riding on the same fixed text, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
