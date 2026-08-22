% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Jati Reification via Colonial Census Classification
 *   domain: social/religious/political-economy
 *
 * SUMMARY:
 *   The colonial census reading instantiates jati categories as a legal,
 *   externally-enforced classification system for administrative legibility.
 *   Historically, jati boundaries were fluid, negotiated locally, and subject
 *   to occupational mobility, marriage, and ritual specialization. The
 *   colonial apparatus froze this fluidity into rigid legal categories used
 *   for taxation, recruitment, and jurisdiction. This reading claims the
 *   constraint is a tangled rope: it solves a genuine coordination problem
 *   (reducing administrative transaction costs across linguistic and ritual
 *   diversity) while extracting occupational constraint and autonomy from
 *   communities. The extraction benefits the colonial apparatus directly and
 *   the brahminical interpreters (whose textual reading becomes
 *   operationalized as law). Sibling readings are the
 *   localized_practice_reading (which emphasizes the coordination aspect and
 *   the community authority to negotiate boundaries) and the
 *   orthodox_textual_reading (which argues the categories were always fixed
 *   in scripture, not created by the census). This story describes the census
 *   reading specifically: the constraint as externally-imposed reification of
 *   categories that were previously fluid.
 *
 * KEY AGENTS:
 *   - colonial_administrative_apparatus: institutional power, arbitrage exit — sets the legal categories and enforces them through courts and census enumeration
 *   - brahminical_orthodox_interpreters: powerful, mobile exit — gain institutional legitimacy when their textual reading is codified into law
 *   - mobile_occupational_groups: moderate power, identity-locked exit — lose occupational mobility when categories are frozen
 *   - boundary_fluid_communities: powerless, trapped exit — lose local authority to negotiate jati boundaries
 *   - ritual_specialists_outside_varna: powerless, trapped exit — are criminalized or forced into inappropriate categories
 *   - localized_jati_assemblies: organized power, constrained exit — are structurally sidelined by courts and census
 *   - reform_movements: organized power, constrained exit — press for abolition or voluntary association
 *   - demographic_researchers: organized, analytical — document the historical fluidity and the construction of rigidity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.68).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.74).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Jati Reification via Colonial Census Classification").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social/religious/political-economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '21f22255-7028-4c55-b015-c63c607692a0').
narrative_ontology:cs_kernel_codification('21f22255-7028-4c55-b015-c63c607692a0', formalized).
narrative_ontology:cs_authority_grounding('21f22255-7028-4c55-b015-c63c607692a0', extraction).
narrative_ontology:cs_interpretation_layer_present('21f22255-7028-4c55-b015-c63c607692a0').
narrative_ontology:cs_reading_relation('21f22255-7028-4c55-b015-c63c607692a0', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_reading_relation('21f22255-7028-4c55-b015-c63c607692a0', jati_practice_norm__orthodox_textual_reading, forecloses).
narrative_ontology:cs_axiom('21f22255-7028-4c55-b015-c63c607692a0', foundational, jati_categories_are_externally_reified).
narrative_ontology:cs_axiom_status(jati_categories_are_externally_reified, holdable).
narrative_ontology:cs_axiom_grounding('21f22255-7028-4c55-b015-c63c607692a0', jati_categories_are_externally_reified, empirically_contingent).
narrative_ontology:cs_axiom('21f22255-7028-4c55-b015-c63c607692a0', secondary, administrative_legibility_requires_fixed_classification).
narrative_ontology:cs_axiom_status(administrative_legibility_requires_fixed_classification, holdable).
narrative_ontology:cs_axiom_grounding('21f22255-7028-4c55-b015-c63c607692a0', administrative_legibility_requires_fixed_classification, instrumental).
narrative_ontology:cs_reference_frame('21f22255-7028-4c55-b015-c63c607692a0', fluid_locally_negotiated_jati_boundaries).
narrative_ontology:cs_drift_state('21f22255-7028-4c55-b015-c63c607692a0', post_colonial_census_institutionalization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('21f22255-7028-4c55-b015-c63c607692a0', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, brahminical_orthodox_interpreters).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, mobile_occupational_groups).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, boundary_fluid_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, ritual_specialists_outside_varna).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, varna_framework_encompasses_all_practice).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, jati_correspondence_to_scriptural_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutes census enumeration and legal classification of jati categories to systematize taxation, recruitment, and governance. Justifies the freezing of categories as clarifying a pre-existing framework; actually imposes rigid external enforcement on what was previously fluid. Benefits from legibility and administrative efficiency: a fixed taxonomy reduces transaction costs and enables targeted revenue collection and labor mobilization.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain institutional authority and legitimacy when the colonial apparatus codifies the varna framework into law and census categories. The textual reading they defend becomes operationalized as state policy. Benefit from the enforcement machinery's suppression of alternative readings and boundary-fluid practices. Positioned to adjudicate disputes about proper classification.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, brahminical_orthodox_interpreters, beneficiary,
    powerful, generational, mobile, national).

% Historically moved between occupational identities and jati affiliations based on circumstance, marriage, migration, and ritual specialization. The census freezes them into a single legal category inherited by their children. Exit means renouncing community kinship, which is identity-constitutive. Their labor value can now be extracted via targeted taxation and legal prohibition on occupational mobility — the administrative apparatus knows exactly where to collect and whom to constrain.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, mobile_occupational_groups, payer,
    moderate, biographical, identity_locked, regional).

% Communities whose jati identity was negotiated locally and changed across generations now have a fixed legal designation. They cannot renegotiate boundaries with neighboring communities as they could before. The census category becomes a law; local practices that deviated from it are criminalized. Geographic isolation and legal disability prevent exit.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, boundary_fluid_communities, payer,
    powerless, biographical, trapped, local).

% Tantriks, healers, ascetics, and specialists in ritual practice that do not fit the varna taxonomy are forced into the nearest legal category or relegated to criminal/outcast status. Their expertise is delegitimized. The census apparatus treats them as anomalies requiring correction rather than as legitimate practitioners with their own authority structure.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, ritual_specialists_outside_varna, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, ritual_specialists_outside_varna, excluded).

% Village and regional gatherings that historically negotiated jati boundaries, resolved disputes, and admitted new members are structurally sidelined. The colonial courts and census apparatus replace them as the authority source. They would argue for restoration of local boundary-setting; they are kept out of the decision-making apparatus by design.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, localized_jati_assemblies, excluded,
    organized, biographical, constrained, local).

% Nineteenth and twentieth-century movements (brahmo samaj, arya samaj, independence movements) critique the entire structure and press for reimagining jati as voluntary association or eliminating it entirely. They observe that the census codification actually created much of what defenders call pre-existing natural law. Their testimony is external to the apparatus and to orthodox defense.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, reform_movements, observer,
    organized, generational, constrained, national).

% Historians and anthropologists document the fluidity of jati boundaries before census codification and the sudden freezing afterward. Their reconstruction shows the constraint was not discovered but constructed. They have access to pre-census records showing boundary negotiation and occupational mobility.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, demographic_researchers, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the administrative problem of making a vast, linguistically and ritually diverse subcontinent legible for taxation and recruitment without maintaining large local knowledge corps. Fixed jati categories reduce the transaction cost of identifying who owes tax, who can be conscripted, and how to organize labor recruitment and allocation. Communities benefit from having a stable legal identity that protects their occupation from sudden exclusion and gives them legal standing in disputes.
% TRANSFER_FUNCTION: Moves occupational constraint, legal disability, and boundary-setting authority FROM mobile occupational groups and boundary-fluid communities TO the colonial administrative apparatus (in the form of taxable income, conscriptable labor, and governed population) and to brahminical orthodox interpreters (in the form of institutional legitimacy for their textual reading now operative as law). Labor mobility, local authority, and identity fluidity are extracted and replaced with fixed legal status.
% ABSENT_VOICES: Localized jati assemblies that historically adjudicated boundary disputes and admitted new members are structurally excluded from the census-and-court apparatus. Ritual specialists and practitioners outside the varna taxonomy would object to being forced into inappropriate legal categories or criminalized — they are excluded by design. Reform movements arguing for abolition or voluntary association are present but treated as external critics who do not get seats at the apparatus. Communities themselves would argue for restoration of local boundary-setting authority, but that authority is no longer recognized by the apparatus that now governs.
% DISAPPEARANCE_RATIONALE: If colonial census enforcement and legal codification of jati categories disappeared, communities would resume local negotiation of boundaries, occupational mobility would become possible again, ritual specialists would be reintegrated into their own authority structures, and local jati assemblies would regain adjudicatory function. The administrative apparatus would lose the legibility it depended on; taxation and recruitment would become more costly to administer and would require negotiation with local authorities rather than application of fixed categories.
% FOUNDING_PROBLEM: The founding problem is administrative legibility: how to govern and extract revenue from a vast, linguistically fragmented, ritually diverse subcontinent without maintaining a prohibitively expensive apparatus of local knowledge specialists. The solution is to impose a externally-legible classification system (the census categories) and use it to replace local negotiation with legal codification.
% FOUNDING_PROBLEM_CORROBORATION: Colonial administrators' own reports and correspondence attest the founding problem: the high cost and inefficiency of collecting taxes and conscripting labor without a standardized classification. Demographic historians and anthropologists, drawing on pre-census records, attest that the problem was solved not by discovering pre-existing categories but by imposing reification on previously fluid categories. The apparatus was genuinely motivated by administrative need; the freezing and reification were the response. From outside the apparatus, researchers can confirm the fluidity preceded the reification — no external source corroborates that the categories were pre-existing natural law that the census merely discovered.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.68 over the interval because the apparatus gradually hardens enforcement and deepens integration into law and land records. Early years see negotiation and resistance; later years show stabilization as colonial courts back the frozen categories and jati becomes a legal fact rather than a practice. Suppression rises from 0.45 to 0.74 as local boundary-setting assemblies are formally abolished and legal penalties are added for violations or reclassification. Theater rises from 0.20 to 0.42 because the apparatus increasingly justifies the frozen categories as discovering pre-existing natural law (the orthodox textual reading), when in fact the reification is the constraint's primary effect. Resistance falls from 0.61 to 0.52 (structural level) as communities internalize the legal categories across generations — the suppression becomes normalized. Accessibility of alternatives collapses from 0.45 to 0.74 at the structural level because the apparatus closes off local renegotiation and makes exit from a census category legally and socially impossible. Stakes inflation reaches 0.76 because violating one's fixed jati category becomes a legal crime, not merely a social deviation. The shared time grid ensures every metric is authored at every examined time point — no misalignment between series.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (colonial apparatus), the constraint is genuine coordination: a real administrative problem is solved by fixing categories, and communities gain stability and legal protection. From the payer seats (mobile occupational groups, boundary-fluid communities, ritual specialists), the same constraint is enforced extraction: their autonomy is taken away, their labor is made legible for taxation, and their authority to negotiate is stripped. From the brahminical interpreters' seat, the constraint vindicates their textual reading as law. The engine computes per-seat classifications from the structural data — beneficiary/victim declarations + power + exit + scope — and should show the apparatus and interpreters perceiving a rope (coordination benefit), while payers perceive a snare (pure extraction with enforcement). The authored claim is tangled_rope, which declares the structure as containing both functions simultaneously, not that both seats agree.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial apparatus directionality: d ≈ 0.1 (full beneficiary — collects extraction revenue, gains administrative efficiency, faces no exit barrier). Brahminical interpreters: d ≈ 0.15 (beneficiary — gain institutional legitimacy and authority, mobile exit means they could step away but choose not to because the constraint vindicates them). Mobile occupational groups: d ≈ 0.75 (near full target — bear occupational constraint, lose mobility, identity-locked prevents exit). Boundary-fluid communities: d ≈ 0.85 (near full target — lose local authority, face legal penalties, trapped exit). Ritual specialists: d ≈ 0.88 (near full target — criminalized, forced into inappropriate categories, trapped with no alternative identity structure recognized). The directionality derivation flows from beneficiary status (apparatus, interpreters) and victim status (groups, communities, specialists) + exit modulation: identity-locked and trapped targets sit nearer d=1.0 than mobile or arbitrage beneficiaries sit near d=0.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem from the colonial reading is administrative legibility for taxation and recruitment. The problem is live in the sense that colonial governance continuously depends on census categories. However, the reformers and researchers attest that the problem was NOT pre-existing: jati boundaries were fluid and locally renegotiated before census codification. The constraint SOLVES a problem the apparatus created (the need for external legibility) by CREATING the very structure it claims to discover. This is a classic mandatrophy pattern: the founding problem is an artifact of the constraint's own operation, not a prior condition the constraint was built to address. The theater metric rises to 0.42 because the apparatus increasingly performs discovery ('we found these natural categories in the texts') when it is actually imposing codification ('we froze what was fluid to make it legible'). The constraint should compute as tangled_rope (it does coordinate and extract simultaneously) but the coordination part is increasingly theatrical — the real function becomes extraction, and the coordination framing becomes cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fluidity_vs_natural_law_ambiguity,
    'Were jati categories pre-existing fixed entities that the census discovered, or were they fluid and locally negotiated until the census froze them?',
    'Archival evidence from pre-census records (village records, legal documents, marriage records) showing boundary negotiation and occupational mobility; comparison with post-census records showing sudden rigidity.',
    'If fluidity is established, the constraint is constructed (tangled_rope: reification is active extraction). If natural law is established, the constraint might be mountain (reification merely recognizes what always was). The empirical case strongly supports fluidity; this omega documents the residual alternative framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fluidity_vs_natural_law_ambiguity, empirical, 'Whether jati categories were naturally fixed or historically fluid until external reification.').

omega_variable(
    identity_lock_mechanism,
    'Is the suppression of occupational mobility in ''mobile_occupational_groups'' structural (legal barriers making exit impossible) or internalized (communities believe they cannot leave because it would violate identity)?',
    'Post-colonial data: do communities that lose legal enforcement of jati barriers still maintain occupational segregation? If barriers are removed and mobility resumes, suppression is primarily structural; if segregation persists despite barrier removal, suppression is partially internalized.',
    'If structural dominates, the constraint''s effective extraction is limited by the barrier removal. If internalized dominates, the constraint''s suppression persists after the apparatus is dismantled — the target carries it forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether census-imposed category lock persists as internalized identity or collapses with barrier removal.').

omega_variable(
    coordination_necessity_ambiguity,
    'Is the administrative coordination function (reducing transaction costs for taxation and recruitment) inherently dependent on category reification, or could the same coordination be achieved through decentralized alternatives?',
    'Counterfactual analysis: other colonial-era contexts that solved administrative legibility without freezing local categories (e.g., indirect rule through local headmen, polling tax without classification). If alternatives exist, reification is not necessary for coordination.',
    'If reification is necessary, the coordination function is a real explanation for the constraint''s persistence. If alternatives exist, the reification is extraction pursued for administrative convenience, not unavoidable coordination need.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_ambiguity, conceptual, 'Whether category reification is structurally necessary for administrative coordination or merely convenient.').

omega_variable(
    brahminical_capture_extent,
    'Did brahminical interpreters drive the census codification as a strategy to freeze their textual reading into law, or was the apparatus''s independent administrative need the primary driver?',
    'Historical analysis of who initiated census categories and classification schemes; role of brahminical advisors vs. independent administrative design; correspondence and records of the apparatus.',
    'If interpreters captured the process, they are primary beneficiaries and the constraint is snare-like (pure extraction for a faction). If apparatus acted independently, interpreters are secondary beneficiaries (alignment rather than capture) and the constraint remains tangled_rope (coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_capture_extent, empirical, 'Whether brahminical beneficiaries drove the constraint''s creation or merely benefited from independent administrative needs.').

omega_variable(
    reading_committer_frame,
    'This story instantiates the colonial_census_reading: the constraint as externally-imposed reification for administrative legibility. Are the sibling readings (localized_practice_reading and orthodox_textual_reading) genuinely coexisting live positions held by different parties, or does the colonial reading''s core premise (that reification is the operative constraint) foreclose the siblings?',
    'Structural logical analysis: the colonial reading holds that the constraint IS the external freezing of categories. The localized_practice reading holds that jati ARE continuous local negotiations (the freezing is secondary, imposed on top of the primary coordination function). The orthodox reading holds that jati ARE fixed in scripture (the reification merely recognizes what always was). These three framings occupy different commitment structures (colonial apparatus, localized communities, brahminical textuality) and no single framework could hold all three as equally true descriptions of what the constraint IS.',
    'The sibling readings coexist (each read is plausible from its own seat) but the colonial reading forecloses the orthodox reading within the SAME framework (if reification is imposed from outside, then scriptural fixity cannot be the origin of the categories). Within different frameworks (colonial apparatus vs. brahminical tradition), both can coexist because they address different questions. This omega documents the committer structure and the foreclosure relation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_frame, conceptual, 'Structural relation between the colonial_census_reading and its sibling readings: foreclosure vs. coexistence by framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(jati_tr_t0, observed).
narrative_ontology:measurement(jati_tr_t5, jati_practice_norm__colonial_census_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(jati_tr_t5, observed).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__colonial_census_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(jati_tr_t10, observed).
narrative_ontology:measurement(jati_tr_t15, jati_practice_norm__colonial_census_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(jati_tr_t15, observed).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__colonial_census_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(jati_tr_t20, observed).
narrative_ontology:measurement(jati_tr_t25, jati_practice_norm__colonial_census_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(jati_tr_t25, observed).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__colonial_census_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(jati_tr_t30, observed).
narrative_ontology:measurement(jati_tr_t35, jati_practice_norm__colonial_census_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(jati_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(jati_be_t0, observed).
narrative_ontology:measurement(jati_be_t5, jati_practice_norm__colonial_census_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(jati_be_t5, observed).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__colonial_census_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(jati_be_t10, observed).
narrative_ontology:measurement(jati_be_t15, jati_practice_norm__colonial_census_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(jati_be_t15, observed).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__colonial_census_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(jati_be_t20, observed).
narrative_ontology:measurement(jati_be_t25, jati_practice_norm__colonial_census_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(jati_be_t25, observed).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__colonial_census_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(jati_be_t30, observed).
narrative_ontology:measurement(jati_be_t35, jati_practice_norm__colonial_census_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(jati_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(jati_su_t0, observed).
narrative_ontology:measurement(jati_su_t5, jati_practice_norm__colonial_census_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement_basis(jati_su_t5, observed).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__colonial_census_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(jati_su_t10, observed).
narrative_ontology:measurement(jati_su_t15, jati_practice_norm__colonial_census_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(jati_su_t15, observed).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__colonial_census_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(jati_su_t20, observed).
narrative_ontology:measurement(jati_su_t25, jati_practice_norm__colonial_census_reading, suppression_requirement, 25, 0.73).
narrative_ontology:measurement_basis(jati_su_t25, observed).
narrative_ontology:measurement(jati_su_t30, jati_practice_norm__colonial_census_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement_basis(jati_su_t30, observed).
narrative_ontology:measurement(jati_su_t35, jati_practice_norm__colonial_census_reading, suppression_requirement, 35, 0.74).
narrative_ontology:measurement_basis(jati_su_t35, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(jati_grid_01, jati_practice_norm__colonial_census_reading, accessibility_collapse(class), 0, 0.38).
narrative_ontology:measurement(jati_grid_02, jati_practice_norm__colonial_census_reading, accessibility_collapse(class), 35, 0.68).
narrative_ontology:measurement(jati_grid_03, jati_practice_norm__colonial_census_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(jati_grid_04, jati_practice_norm__colonial_census_reading, accessibility_collapse(individual), 35, 0.72).
narrative_ontology:measurement(jati_grid_05, jati_practice_norm__colonial_census_reading, accessibility_collapse(organizational), 0, 0.42).
narrative_ontology:measurement(jati_grid_06, jati_practice_norm__colonial_census_reading, accessibility_collapse(organizational), 35, 0.78).
narrative_ontology:measurement(jati_grid_07, jati_practice_norm__colonial_census_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(jati_grid_08, jati_practice_norm__colonial_census_reading, accessibility_collapse(structural), 35, 0.74).
narrative_ontology:measurement(jati_grid_09, jati_practice_norm__colonial_census_reading, resistance(class), 0, 0.56).
narrative_ontology:measurement(jati_grid_10, jati_practice_norm__colonial_census_reading, resistance(class), 35, 0.45).
narrative_ontology:measurement(jati_grid_11, jati_practice_norm__colonial_census_reading, resistance(individual), 0, 0.48).
narrative_ontology:measurement(jati_grid_12, jati_practice_norm__colonial_census_reading, resistance(individual), 35, 0.42).
narrative_ontology:measurement(jati_grid_13, jati_practice_norm__colonial_census_reading, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(jati_grid_14, jati_practice_norm__colonial_census_reading, resistance(organizational), 35, 0.38).
narrative_ontology:measurement(jati_grid_15, jati_practice_norm__colonial_census_reading, resistance(structural), 0, 0.61).
narrative_ontology:measurement(jati_grid_16, jati_practice_norm__colonial_census_reading, resistance(structural), 35, 0.52).
narrative_ontology:measurement(jati_grid_17, jati_practice_norm__colonial_census_reading, stakes_inflation(class), 0, 0.35).
narrative_ontology:measurement(jati_grid_18, jati_practice_norm__colonial_census_reading, stakes_inflation(class), 35, 0.71).
narrative_ontology:measurement(jati_grid_19, jati_practice_norm__colonial_census_reading, stakes_inflation(individual), 0, 0.32).
narrative_ontology:measurement(jati_grid_20, jati_practice_norm__colonial_census_reading, stakes_inflation(individual), 35, 0.68).
narrative_ontology:measurement(jati_grid_21, jati_practice_norm__colonial_census_reading, stakes_inflation(organizational), 0, 0.28).
narrative_ontology:measurement(jati_grid_22, jati_practice_norm__colonial_census_reading, stakes_inflation(organizational), 35, 0.62).
narrative_ontology:measurement(jati_grid_23, jati_practice_norm__colonial_census_reading, stakes_inflation(structural), 0, 0.42).
narrative_ontology:measurement(jati_grid_24, jati_practice_norm__colonial_census_reading, stakes_inflation(structural), 35, 0.76).
narrative_ontology:measurement(jati_grid_25, jati_practice_norm__colonial_census_reading, suppression(class), 0, 0.48).
narrative_ontology:measurement(jati_grid_26, jati_practice_norm__colonial_census_reading, suppression(class), 35, 0.79).
narrative_ontology:measurement(jati_grid_27, jati_practice_norm__colonial_census_reading, suppression(individual), 0, 0.38).
narrative_ontology:measurement(jati_grid_28, jati_practice_norm__colonial_census_reading, suppression(individual), 35, 0.71).
narrative_ontology:measurement(jati_grid_29, jati_practice_norm__colonial_census_reading, suppression(organizational), 0, 0.42).
narrative_ontology:measurement(jati_grid_30, jati_practice_norm__colonial_census_reading, suppression(organizational), 35, 0.76).
narrative_ontology:measurement(jati_grid_31, jati_practice_norm__colonial_census_reading, suppression(structural), 0, 0.45).
narrative_ontology:measurement(jati_grid_32, jati_practice_norm__colonial_census_reading, suppression(structural), 35, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__colonial_census_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel is instantiated by three readings: colonial_census_reading (this story, external reification for administrative legibility), localized_practice_reading (jati as continuous local negotiation and coordination), and orthodox_textual_reading (jati as fixed scriptural varna categories). Each reading is a separate constraint story with its own ε, beneficiary/victim structure, and classification. The colonial reading is most extractive (tangled_rope) because it freezes fluidity for external administrative benefit. The localized reading is less extractive (rope or scaffold — pure coordination with local autonomy) because it emphasizes community authority and renegotiation. The orthodox reading appears as mountain (natural law) or false summit (if beneficiaries are shown) because it claims scriptural fixity. All three are linked by network.affects_constraints to model the kernel's internal structure: the colonial reading influences (creates structural pressure on) the localized reading by imposing legal barriers to the local renegotiation the reading depends on. The orthodox reading coexists with both because different parties (brahminical interpreters, anthropologists, communities) hold different readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
