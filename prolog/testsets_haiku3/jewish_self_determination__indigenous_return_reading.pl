% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigenous Return and Zionist Decolonization (Indigenous Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'jewish_self_determination' — it is not a description of historical fact
 *   or political reality independent of framing. This reading instantiates
 *   the indigenous-return narrative: the claim that Jewish people possess
 *   unbroken historical and religious connection to the land, making
 *   political sovereignty a decolonization of indigenous rights rather than
 *   colonization or displacement. The reading coordinates Jewish identity,
 *   historical narrative, and postcolonial legitimacy categories into a
 *   unified claim. High extractiveness reflects the reading's dependence on a
 *   contested historical premise (unbroken connection) whose acceptance is
 *   suppressed by active historiographical disputation — the reading must
 *   suppress alternative historical framings (Palestinian continuity, Jewish
 *   diaspora severing connection, competing indigenous claims) to maintain
 *   its coherence. Theater rises over the interval as the reading shifts from
 *   historical-proof emphasis (early period) toward legitimacy-defense
 *   emphasis (later period) — performing indigenous authenticity and
 *   continuous identity becomes more central as empirical historical
 *   contestation intensifies.
 *
 * KEY AGENTS:
 *   - Jewish claimants to ancestral land — benefit from the reading's framing of Zionism as decolonization; identity-locked to the narrative framework
 *   - Palestinian populations and advocates — excluded from the core dispute within the reading; their competing indigenous claims are reframed as subordinate or later-arriving
 *   - Diasporist Jewish critics — positioned as opponents of Jewish self-determination; bear social/institutional costs within Jewish communities
 *   - Historians and archaeologists — occupy analytical seat; produce evidence that contests the reading's core claim of unbroken connection
 *   - International law bodies — assess whether the reading meets legal criteria for indigeneity and decolonization
 *   - Religious Zionist movement — benefits from and co-sets the reading, grounding it in theological commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.72).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.68).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigenous Return and Zionist Decolonization (Indigenous Reading)").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '9f8e1dfb-9ed0-43a8-8f2e-831016663e62').
narrative_ontology:cs_kernel_codification('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', fixed_text).
narrative_ontology:cs_authority_grounding('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', lineage).
narrative_ontology:cs_interpretation_layer_present('9f8e1dfb-9ed0-43a8-8f2e-831016663e62').
narrative_ontology:cs_reading_relation('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', foundational, jewish_people_indigenous_unbroken_connection).
narrative_ontology:cs_axiom_status(jewish_people_indigenous_unbroken_connection, holdable).
narrative_ontology:cs_axiom_grounding('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', jewish_people_indigenous_unbroken_connection, empirically_contingent).
narrative_ontology:cs_axiom('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', secondary, indigenous_self_determination_rights_legitimate).
narrative_ontology:cs_axiom_status(indigenous_self_determination_rights_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', indigenous_self_determination_rights_legitimate, deontological).
narrative_ontology:cs_reference_frame('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', jewish_historical_presence_and_land_connection).
narrative_ontology:cs_drift_state('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', contemporary_historiographical_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9f8e1dfb-9ed0-43a8-8f2e-831016663e62', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, religious_zionist_movement).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, diasporist_jewish_critics).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, jewish_historical_presence_and_continuity).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, indigenous_self_determination_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities globally claim historical and unbroken connection to the land, legitimating the establishment of Jewish political sovereignty as recovery of indigenous rights after two millennia of diaspora and persecution. The reading positions this claim as decolonization — the return of an indigenous people to their historical homeland — rather than as conquest or displacement. Their exit from this reading is identity-fused: rejecting the ancestral connection requires rejecting core religious and cultural identity frameworks.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, beneficiary,
    organized, civilizational, identity_locked, global).

% Jewish intellectuals, movements, and communities who argue that Jewish flourishing is secured through diaspora pluralism and minority rights, not territorial sovereignty. They pay a cost by being positioned as opponents of Jewish self-determination within their own communities, and face social pressure and institutional marginalization. Their exit is feasible: they can adopt alternative Jewish political philosophies or relocate their organizing to diasporist frameworks outside the constraints of this reading.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, diasporist_jewish_critics, payer,
    organized, biographical, mobile, global).

% Palestinians and their international advocates are structurally excluded from the core dispute within this reading — the reading treats Palestinian presence as either later arrival, co-indigenous with subordinate claim, or a separate political problem orthogonal to the Jewish indigenous question. They cannot voice the core objection: that the reading's framing of unbroken Jewish connection erases or minimizes Palestinian historical presence, continuous habitation, and competing indigenous claims. Their exit is trapped: they cannot leave the territory or the dispute.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_populations_and_advocates, excluded,
    moderate, biographical, trapped, regional).

% Academic communities studying archaeological evidence, textual sources, and settlement patterns produce competing evidence for the extent, continuity, and significance of Jewish presence in the region across different historical periods. They occupy an analytical seat from which they examine the evidentiary grounding of the reading's core claims about unbroken connection and historical priority.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, historians_and_archaeologists, observer,
    institutional, generational, analytical, global).

% UN bodies, regional human rights institutions, and international legal scholarship examine whether the reading's indigenous-return framing meets established legal criteria for indigenous peoples' rights, self-determination, and decolonization under international law. They assess the reading's legality claims.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_law_bodies, observer,
    institutional, generational, analytical, global).

% Religious movements that ground Jewish claims to the land in divine covenant and religious obligation. They benefit from the indigenous-return reading's validation of territorial sovereignty but ground it differently — in theological commitment rather than historical-secular indigeneity. They help set the terms of the reading by embedding religious validation into the indigenous-return narrative.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, religious_zionist_movement, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, religious_zionist_movement, agenda_setter).

% Scholars of settler colonialism analyze whether the reading's frame as decolonization squares with the structural properties of settler-colonial arrangements: population replacement, legal exclusion, security apparatus, land dispossession. They apply competing analytical frameworks that would yield different verdicts on the reading.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, settler_colonial_theorists, observer,
    institutional, biographical, analytical, global).

% Proponents of liberal nationalism who argue Jewish people have equal claim to self-determination as other peoples, on grounds of peoplehood and political community rather than indigeneity claims. They coexist with this reading but frame the justification differently.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, liberal_nationalists, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for understanding Jewish political sovereignty as legitimate recovery of indigenous rights, framing Zionism as decolonization rather than colonization. Coordinates Jewish identity, historical narrative, and political claim-making into a unified indigeneity narrative that justifies territorial self-determination.
% TRANSFER_FUNCTION: Transfers legitimacy claims from the category of indigenous peoples' rights — established in postcolonial theory and international law as morally and legally privileged — to Jewish claims for territorial sovereignty. The arrangement moves the burden of proof: instead of defending Jewish settlement as ethically justified, the reading reverses the frame to demand critics of Zionism justify why this indigenous people should be excluded from self-determination rights.
% ABSENT_VOICES: Palestinians and Palestinian historians who would object that this reading erases or subordinates their own continuous presence and historical claim to indigeneity in the same territory. Diasporist Jews who argue that Jewish flourishing is not tied to territorial sovereignty are partly excluded by being positioned as opponents of Jewish self-determination. Historians and archaeologists whose research identifies competing interpretations of the evidence for unbroken Jewish connection are excluded from the core premise-setting of the reading.
% DISAPPEARANCE_RATIONALE: If this reading (the claim that Jewish people are indigenous with unbroken connection, making Zionism decolonization) disappeared overnight, the world would not rearrange itself — the political reality of Israeli statehood, Palestinian displacement, and ongoing territorial disputes would persist. What would vanish is a particular LEGITIMACY NARRATIVE: the framing that positions Jewish sovereignty as indigenous recovery. Political arrangements would reorganize around alternative justifications (liberal nationalism, religious covenant, power-political realism) or alternative readings (settler colonialism, diasporist pluralism). The reading's disappearance would not erase the territorial facts; it would strip away one particular normative frame for understanding them.
% FOUNDING_PROBLEM: After two millennia of diaspora, persecution, and statelessness, Jewish communities sought political sovereignty and national homeland. The founding problem is: how to justify this territorial claim in a postcolonial framework where colonialism is illegitimate but indigenous peoples' self-determination rights are recognized as legitimate? The reading solves this by reframing Jewish return as indigenous recovery rather than colonization.
% FOUNDING_PROBLEM_CORROBORATION: Jewish communities, Israeli historians, and religious Zionist movements affirm the founding problem and its status as live and urgent. Diasporist critics and Palestinian scholars dispute whether territorial sovereignty is the appropriate or necessary response to historical persecution and diaspora. International legal scholars dispute whether the reading's application of indigeneity criteria meets established standards. No corroboration exists from outside the benefiting parties — Palestinian scholars, diasporist critics, and settler-colonial theorists all contest both the problem statement and the reading's resolution.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the reading's coherence depends on suppressing competing historical interpretations and Palestinian counter-claims. The core claim — unbroken Jewish connection — is contested by mainstream historical scholarship that documents significant discontinuities, Palestinian continuous habitation, and competing indigenous narratives. For the reading to persist, these alternative framings must be actively suppressed (not merely disagreed with, but treated as intellectually illegitimate or politically dangerous). Suppression is substantial (0.68) because the reading's maintenance requires institutional control over historical narrative — curriculum, textbooks, academic hiring, media representation — to prevent the counter-narratives from reaching audiences. Theater rises over time (0.25 to 0.42) because as empirical historical contestation increases, the reading shifts from proof-offering to legitimacy-defense and identity-performance: emphasizing Jewish historical presence becomes less central than performing continuous Jewish identity and community bond to the land, which are less empirically falsifiable. Accessibility_collapse is moderate (0.58) because alternatives to the reading (settler-colonial frame, diasporist frame, Palestinian indigeneity) are available and increasingly visible in academic discourse, even if suppressed within certain institutional spheres. Resistance is high (0.79) because substantial populations (Palestinians, diasporist Jews, postcolonial theorists, international human rights bodies) actively contest the reading's core premises and conclusions.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (Jewish claimants), the reading is genuine recovery of indigenous rights — a moral triumph of historical justice and national self-determination after persecution and diaspora. From the excluded seat (Palestinian populations), the same reading operates as legitimacy cover for displacement and land dispossession. From the diasporist critic seat, the reading is a dangerous deviation that ties Jewish fate to a militarized state rather than securing Jewish flourishing through pluralism. From the analytical seat (historians, international law scholars), the reading's factual premises are contested: the empirical claim of unbroken connection, the definitional fit with established indigeneity criteria, and the historical causality of displacement. These perspectival gaps are not reconcilable within the reading itself — they are structural to how the reading distributes costs and legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading's beneficiary (jewish_claimants_to_ancestral_land) sits at the beneficiary end of the directionality spectrum (d near 0.0) — they collect legitimacy and justification from the reading's operation. Their power is organized, their time horizon civilizational, and their exit is identity_locked: rejecting the indigenous-return narrative requires rejecting core frameworks of Jewish historical identity and meaning-making, which is structurally difficult. Diasporist critics (role=payer) sit at the target end (d near 1.0) — they bear costs (social marginalization, institutional pressure) through the reading's operation. Their power is organized but subordinate within Jewish institutional settings; their exit is mobile (they can adopt alternative frameworks). Palestinians (role=excluded) have high d (trapped, cannot exit the territory or dispute) but are structured OUT of the reading's core premise-setting — their objections are excluded from the benefit/cost calculus within the reading itself. This is the structural asymmetry the reading depends on: Palestinian exclusion makes the beneficiary/victim binary appear clean (beneficiaries collecting, no acknowledged victims because victims are excluded from the frame).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT exhibit mandatrophy in the classical sense (founding problem dead but arrangement persists). The founding problem (securing Jewish political sovereignty and protection after diaspora and persecution) remains live — persecution continues, diaspora vulnerabilities persist, Jewish communities continue to experience political insecurity. What has shifted is not the founding problem but the GROUNDING of the legitimacy claim: the reading increasingly depends on suppressing historical contestation (theater ratio rising, extractiveness rising) rather than on proof of unbroken connection (which is itself contested). The reading's early function was historical-proof (documenting Jewish presence); its current function is identity-performance and legitimacy-defense (performing continuous community bond). This signals not mandatrophy but SHIFTING FUNCTIONAL EMPHASIS within a live founding problem — the reading is having to suppress alternatives more aggressively as its empirical premises face greater historical scrutiny. The suppression rise from 0.52 to 0.68 reflects increasing maintenance cost: the reading must work harder to exclude Palestinian claims, diasporist alternatives, and revisionist history from institutional discourse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unbroken_connection_empirical_contestation,
    'Does the historical and archaeological evidence support the claim of unbroken Jewish presence and connection to the land across the entire diaspora period (roughly 70 CE to 1880s)?',
    'Systematic review of archaeological evidence, textual sources, demographic studies, and settlement pattern analysis by scholars outside the benefiting parties. Independent historical research program funded and conducted without institutional pressure toward either reading.',
    'If unbroken connection is substantially supported, the reading''s core factual premise holds and extractiveness drops (premise becomes less contested). If the evidence shows significant discontinuities, Palestinian populations'' competing historical claims appear stronger, and the reading''s extraction mechanism (suppressing alternative histories) becomes more visible — extractiveness may rise further as suppression intensifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unbroken_connection_empirical_contestation, empirical, 'The historical contestation over Jewish presence and continuity.').

omega_variable(
    indigenous_criteria_fit,
    'Does the reading''s application of ''indigenous'' criteria meet established international legal and scholarly definitions of indigenous peoples'' rights, particularly regarding: continuous habitation, prior occupancy, cultural distinctiveness, and historical priority relative to competing claims in the same territory?',
    'UN permanent forum on indigenous issues, international legal scholarship on indigeneity criteria, comparative analysis with other contested indigenous claims (Native Americans, Aboriginal Australians, Sami, etc.). Assessment of whether the reading satisfies ALL criteria or selectively applies some while setting aside others.',
    'If the fit is established, the reading gains legal-normative grounding and its extractiveness could drop (legitimacy secured). If the fit is weak or selective, the reading''s legitimacy claim appears as category-stretching, and theater_ratio rises further (performing legitimacy rather than proving it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_criteria_fit, conceptual, 'Whether indigeneity framing aligns with established legal and theoretical criteria.').

omega_variable(
    palestinian_competing_indigeneity,
    'Do Palestinians have competing legitimate claims to indigeneity in the same territory, and if so, how does the reading''s framework account for or subordinate those claims?',
    'Comparative historical and legal analysis of Palestinian presence, continuity, cultural distinctiveness, and prior occupancy claims using the same criteria applied to Jewish claims. Assessment of whether the reading explicitly addresses dual or competing indigeneity or treats Palestinian presence as derivative.',
    'If Palestinians have competing valid indigeneity claims, the reading''s resolution becomes zero-sum (one indigenous people''s rights preclude the other''s) rather than complementary. This raises the structural extraction: the reading must suppresses Palestinian counter-claims to maintain coherence. Extractiveness and suppression both rise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palestinian_competing_indigeneity, conceptual, 'Whether the reading accounts for competing Palestinian indigeneity or treats it as absent/subordinate.').

omega_variable(
    diasporist_exit_identity_lock,
    'Is the diasporist position truly available as an exit for Jewish communities, or does the reading''s framing of sovereignty as the solution to Jewish persecution and insecurity make diasporist alternatives appear as denial of self-determination and thus psychologically/socially foreclosed?',
    'Ethnographic and sociological study of Jewish communities and institutions under the reading''s dominance: how readily can members adopt diasporist alternatives without experiencing social stigma, institutional marginalization, or identity-challenge? Can a Jew hold diasporist views while maintaining community standing and institutional participation?',
    'If diasporist alternatives are genuinely available exits (people can hold them without severe social cost), the reading''s structure is more clearly extractive (suppressing alternatives for payers while beneficiaries collect). If diasporist alternatives are psychologically/institutionally foreclosed by the reading''s dominance, the suppression mechanism is internalized rather than structural — the payers (diasporist critics) carry the suppression with them as identity anxiety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diasporist_exit_identity_lock, empirical, 'Whether diasporist alternatives are genuinely available or foreclosed by the reading''s identity integration.').

omega_variable(
    kernel_frame_as_reading_property,
    'Is this constraint better understood as a READING of a contested kernel (jewish_self_determination) or as a STANDALONE CLAIM about historical fact? Does the kernel framing obscure the fact that this is a particular interpretation of events, or does it illuminate the reading''s function?',
    'Comparison with how the other sibling readings (settler_colonial, diasporist, liberal_nationalist) are generated and framed. If all siblings are generated as readings of the kernel with omega acknowledgment of contestation, the kernel frame illuminates. If only this reading is treated as a reading while others are treated as claims, the frame obscures asymmetrically.',
    'If the kernel framing is clear and symmetric across siblings, audiences understand the reading as ONE INTERPRETATION rather than as TRUTH. If asymmetric, this reading appears naturalized (treated as fact) while competitor readings appear ideological (treated as interpretation). The framing choice affects how extracted/suppressed alternative readings become.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_frame_as_reading_property, conceptual, 'Whether the kernel frame adequately marks this as reading vs. fact claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__indigenous_return_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t8, jewish_self_determination__indigenous_return_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t8, observed).
narrative_ontology:measurement(jewi_tr_t16, jewish_self_determination__indigenous_return_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(jewi_tr_t16, observed).
narrative_ontology:measurement(jewi_tr_t25, jewish_self_determination__indigenous_return_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t25, observed).
narrative_ontology:measurement(jewi_tr_t34, jewish_self_determination__indigenous_return_reading, theater_ratio, 34, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t34, observed).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__indigenous_return_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__indigenous_return_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t8, jewish_self_determination__indigenous_return_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(jewi_be_t8, observed).
narrative_ontology:measurement(jewi_be_t16, jewish_self_determination__indigenous_return_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(jewi_be_t16, observed).
narrative_ontology:measurement(jewi_be_t25, jewish_self_determination__indigenous_return_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(jewi_be_t25, observed).
narrative_ontology:measurement(jewi_be_t34, jewish_self_determination__indigenous_return_reading, base_extractiveness, 34, 0.71).
narrative_ontology:measurement_basis(jewi_be_t34, observed).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__indigenous_return_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(jewi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__indigenous_return_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t8, jewish_self_determination__indigenous_return_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement_basis(jewi_su_t8, observed).
narrative_ontology:measurement(jewi_su_t16, jewish_self_determination__indigenous_return_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(jewi_su_t16, observed).
narrative_ontology:measurement(jewi_su_t25, jewish_self_determination__indigenous_return_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement_basis(jewi_su_t25, observed).
narrative_ontology:measurement(jewi_su_t34, jewish_self_determination__indigenous_return_reading, suppression_requirement, 34, 0.67).
narrative_ontology:measurement_basis(jewi_su_t34, observed).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__indigenous_return_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement_basis(jewi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__indigenous_return_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'jewish_self_determination'. It is NOT a standalone claim about whether Zionism is good or justified. Sibling readings (settler_colonial, diasporist, liberal_nationalist, religious_covenant) instantiate competing constraints with different epsilon values, beneficiary/victim structures, and extracted/suppressed alternatives. All five readings are generated as separate constraint stories linked via the network. The reading-level divergence (what epsilon means, who collects, what is suppressed) is the measurement the corpus takes: how different framings of the same historical/political situation distribute extraction and legitimacy across seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__indigenous_return_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
