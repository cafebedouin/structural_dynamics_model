% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Medinan Practice ('Amal ahl al-Madina) Source Doctrine
 *   domain: religious/legal/institutional-history
 *
 * SUMMARY:
 *   A methodological regime in formative Islamic law treats the continuous
 *   practice of the Medinan community ('amal ahl al-Madina) as an independent
 *   binding source of law alongside Qur'an and transmitted Hadith, on the
 *   ground that Medina — where the Companions settled en masse — preserved
 *   the Prophet's practice more faithfully than any report chain can attest.
 *   Within the Maliki framework the regime binds legal reasoning: where an
 *   authenticated isolated report conflicts with Medinan practice, the
 *   practice discounts the report. This story instantiates ONE reading of the
 *   contested jurisprudential-method kernel; the committer structure (kernel,
 *   siblings, disagreement location) is recorded in commentary.kernel_context
 *   and the omega variables, not in the constraint itself. The epsilon
 *   referent is the standing arrangement under contest — the Medinan-practice
 *   authority regime as it actually operated — not any rival arrangement; the
 *   reading's own lights fix what the story is about, while the metric value
 *   is authored from the structural record. The constraint carries both a
 *   genuine coordination function (a fabrication-resistant epistemic filter
 *   and a stabilizing shared referent) and an asymmetric extraction structure
 *   (interpretive authority concentrates in the Medinan lineage and its heirs
 *   while non-Medinan analogical claims and hadith-specialist claims are
 *   structurally discounted). Claim and metrics are authored independently:
 *   the claimed type is what the structural analysis supports; the metrics
 *   describe the arrangement's operation across the formative period.
 *
 * KEY AGENTS:
 *   - medinan_scholarly_lineage: Agenda-setter and primary beneficiary (institutional/identity_locked) — administers what counts as 'amal and collects the interpretive authority it confers
 *   - medinan_practicing_community: Beneficiary (moderate/constrained) — its ordinary continuous conduct becomes binding law
 *   - kufan_analogist_jurists: Primary payer (organized/constrained) — its analogical method and local precedent are discounted by the practice-priority claim
 *   - hadith_transmission_specialists: Secondary payer (organized/constrained) — authenticated isolated reports are subordinated to communal practice
 *   - maghribi_andalusian_maliki_jurists: Downstream beneficiary (organized/identity_locked) — inherits the school's authority by affiliation while administering only the codified canon
 *   - dhimmi_subject_communities: Excluded (powerless/trapped) — governed by the law's outputs with no seat in the source-ranking dispute
 *   - legal_method_historians: Analytical observer (analytical/analytical) — sees the full structure of the methodological contest and the transmission record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.58).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.6).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Medinan Practice ('Amal ahl al-Madina) Source Doctrine").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal/institutional-history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '0884325d-a89f-4952-bf30-dce296f5757c').
narrative_ontology:cs_kernel_codification('0884325d-a89f-4952-bf30-dce296f5757c', fixed_text).
narrative_ontology:cs_authority_grounding('0884325d-a89f-4952-bf30-dce296f5757c', lineage).
narrative_ontology:cs_interpretation_layer_present('0884325d-a89f-4952-bf30-dce296f5757c').
narrative_ontology:cs_reading_relation('0884325d-a89f-4952-bf30-dce296f5757c', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('0884325d-a89f-4952-bf30-dce296f5757c', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('0884325d-a89f-4952-bf30-dce296f5757c', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('0884325d-a89f-4952-bf30-dce296f5757c', foundational, medinan_practice_independently_probative).
narrative_ontology:cs_axiom_status(medinan_practice_independently_probative, holdable).
narrative_ontology:cs_axiom_grounding('0884325d-a89f-4952-bf30-dce296f5757c', medinan_practice_independently_probative, empirically_contingent).
narrative_ontology:cs_axiom('0884325d-a89f-4952-bf30-dce296f5757c', foundational, communal_continuity_beats_isolated_report).
narrative_ontology:cs_axiom_status(communal_continuity_beats_isolated_report, holdable).
narrative_ontology:cs_axiom_grounding('0884325d-a89f-4952-bf30-dce296f5757c', communal_continuity_beats_isolated_report, empirically_contingent).
narrative_ontology:cs_axiom('0884325d-a89f-4952-bf30-dce296f5757c', secondary, medinan_transmission_chain_authorized).
narrative_ontology:cs_axiom_status(medinan_transmission_chain_authorized, holdable).
narrative_ontology:cs_axiom_grounding('0884325d-a89f-4952-bf30-dce296f5757c', medinan_transmission_chain_authorized, conventional).
narrative_ontology:cs_reference_frame('0884325d-a89f-4952-bf30-dce296f5757c', medina_continuous_prophetic_practice).
narrative_ontology:cs_drift_state('0884325d-a89f-4952-bf30-dce296f5757c', post_shafii_hadith_canonization, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0884325d-a89f-4952-bf30-dce296f5757c', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_practicing_community).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maghribi_andalusian_maliki_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, kufan_analogist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, hadith_transmission_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The juristic class of Medina — the Successors to the Companions, Malik ibn Anas and his circle, the Medinan muftis — holds that its city's continuous practice transmits the Prophet's sunna. It decides what counts as 'amal, trains the jurists who reproduce the claim, and collects the adjudicative and teaching authority the claim confers. Exit would dissolve the ground of its own authority: a Medinan jurist who abandoned the practice-priority claim would be a jurist with no distinctive warrant, and the school's identity is fused with being the faithful witness of prophetic practice.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary).

% The ordinary Muslims of Medina — merchants, households, litigants before Medinan judges — whose continuous customary conduct is the doctrine's evidentiary base. Their everyday practice acquires binding legal force and the status of living witness to the Prophet's practice. They did not design the doctrine; leaving Medina would forfeit the authority their conduct carries there, since the claim attaches to practice in that place.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_practicing_community, beneficiary,
    moderate, biographical, constrained, local).

% The Kufan juristic class grounds law in local precedent and systematic analogical reasoning (qiyas, istihsan). Under the Medinan-priority claim, Kufa's practice carries no comparable authenticity, and its analogical extensions are discounted wherever they conflict with Medinan practice. Its interpretive capital is place-bound: it cannot relocate Kufa's practice into Medina, and accepting Medinan priority would devalue its own school's method and accumulated case law.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, kufan_analogist_jurists, payer,
    organized, generational, constrained, regional).

% Traditionists across the empire collect and grade isolated reports through isnad criticism. Where a well-attested report conflicts with Medinan practice, the practice-priority doctrine discounts the report — subordinating the specialist's core product to a communal practice they cannot audit chain by chain. Their authority rises only insofar as they can reframe reports as the explanation of the practice rather than its corrective.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hadith_transmission_specialists, payer,
    organized, generational, constrained, continental).

% Jurists of Qayrawan, Cordoba, and the Maghreb inherit the school's authority through affiliation with the Medinan chain of transmission. The practice-priority doctrine grounds their rulings' legitimacy at a distance of thousands of kilometers from the practice itself; what they actually administer is the codified school canon (the Mudawwana and its commentaries). Their professional identity and judicial appointments depend on a preservation claim about a distant city they cannot observe, and abandoning school affiliation would cost them their standing entirely.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maghribi_andalusian_maliki_jurists, beneficiary,
    organized, generational, identity_locked, continental).

% Non-Muslim and non-jurist subjects governed by the law this method produces have no seat in the methodological dispute. The question of which interpretive claims count — and therefore which rulings bind them — is settled entirely among the competing juristic classes. They bear the outputs of the source-ranking without any voice in how the sources are ranked.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, dhimmi_subject_communities, excluded,
    powerless, generational, trapped, regional).

% Historians of Islamic law and usul al-fiqh analysts, from rival methodologists of the formative period to modern academics, examine the transmission record and the schools' contest. They hold no stake in which source ranks first, can see the full structure of the methodological dispute, and can compare the preservation claim against the documentary record.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, legal_method_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Filters and stabilizes legal interpretation: when the Qur'an is silent or ambiguous and transmitted reports conflict, the continuous practice of an entire community serves as a shared, fabrication-resistant referent that no single report chain and no individual jurist's reasoning can easily counterfeit — giving dispersed judges and jurists one testable standard for what the Prophet's practice actually was.
% TRANSFER_FUNCTION: Moves interpretive and adjudicative authority — teaching chairs, judicial appointments, the power to certify rulings as sound — from jurists whose claims rest on analogical extension or isolated report transmission to the Medinan scholarly lineage and its affiliated heirs, along with the school allegiance and endowments that follow that authority.
% ABSENT_VOICES: The Kufan jurists contested Medinan priority but stood outside the Medinan consensus-setting process in which the doctrine was formulated; hadith specialists outside the school could not audit 'amal claims chain by chain and were answered with the practice itself; and the subject populations governed by the resulting law, including non-Muslim communities, had no seat at all in the dispute that ranked the sources binding them.
% DISAPPEARANCE_RATIONALE: If 'amal ahl al-Madina lost its status as an independent source overnight, the Maliki school's distinctive rulings would lose their independent ground, its authority structure would collapse into a transmission-hierarchy framework of the Shafi'i type, and non-Medinan claims to authenticity would regain equal standing — the four-reading landscape of Islamic jurisprudence would reorganize around three, and Maghribi legal institutions would need to re-derive their legitimacy from transmission alone.
% FOUNDING_PROBLEM: After the Prophet's death the community had to determine God's law in cases the Qur'an did not address, and transmitted reports conflicted with one another. The founding problem was how to identify authentic prophetic practice when reports can be fabricated, misremembered, or contested: Medina's continuous communal practice was proposed as the witness hardest to forge, because the Companions settled there en masse and the practice of a whole city, unlike a report, cannot be manufactured by a single transmission chain.
% FOUNDING_PROBLEM_CORROBORATION: Rival schools corroborate the problem while rejecting this reading's solution: al-Shafi'i's al-Risala and al-Umm attest the report-conflict problem and argue that authenticated transmission, not communal continuity, must arbitrate; Hanafi usul works attest the same problem and offer systematic analogy as the answer. Modern historians of Islamic law corroborate that the problem was real and that the 'amal claim functioned as a school-constituting device. Corroboration of the problem from outside the beneficiary set is abundant; corroboration of the Medinan solution itself from outside the beneficiary set is not.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is medium (0.58) because the authority transfer is real and structural — non-Medinan claims are discounted by design — but the epistemic filter function is genuine rather than cover: a whole community's continuous practice IS harder to fabricate than a single report chain, so part of the arrangement's cost is the price of a real coordination good. Suppression (0.60) is authored as a raw structural property, unscaled: enforcement is institutional (refutation literature defending 'amal against the transmission-hierarchy critique, appointment and teaching networks that reproduce school allegiance, boundary maintenance against rival methods), with an internalized component carried by identity-fused jurists (see omega suppression_mechanism_ambiguity). Theater (0.45) is the story's clearest drift signal: at interval start the 'amal is lived practice; by interval end the 'living tradition' is largely the codified Mudawwana, and appeals to Medinan practice are appeals to school canon — the theater series rises monotonically with codification. Accessibility_collapse (0.45) is moderate: the constraint does not collapse its alternatives, it subordinates them within its own jurisdiction — rival readings persist and remain live across the landscape, which is why resistance (0.60) is substantial: the transmission-hierarchy critique attacked this constraint directly and sustainedly, and the Kufan school contested Medinan priority throughout. All three metric series are authored on one shared six-point grid (700/760/820/880/940/1000); the extraction trajectory rises as the claim hardens from epistemic filter into school boundary, peaking in enforcement intensity during the direct methodological contest of the early ninth century and plateauing as inter-school coexistence institutionalizes.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the Medinan lineage seat the arrangement is faithful epistemic housekeeping — a rope-like filter that protects the Prophet's practice from fabrication, with extraction experienced as negligible because the lineage both administers the standard and is its measure. From the Kufan and hadith-specialist seats the same structure operates as authority monopolization under a pious cover story — snare-flavored — because the discounting of their claims is the enforcement object itself. From the Maghribi heir seat it is inherited legitimacy: a benefit they cannot verify at the source and cannot abandon without identity loss, which is why their exit is identity_locked rather than mobile. Identity-lock here is professional and institutional: career and teaching path dependence fused with an organization that has 'become' its function as the faithful witness of prophetic practice; if that identity frame broke, the lineage's exit options would widen and the payer seats' effective extraction would fall. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Medinan lineage sits near the beneficiary end despite holding agenda-setting power: it both administers the standard and collects its rents, and its identity-lock amplifies rather than offsets the subsidy. The Medinan practicing community sits low-d (genuine status benefit, diffuse cost). The Maghribi jurists sit low-to-moderate d: beneficiaries by affiliation, though structurally exposed to any failure of the preservation claim they inherit. The Kufan analogists and hadith specialists sit near the target end — the doctrine's operation consists precisely in discounting their claims, and their exits are constrained by the place-bound or product-bound nature of their interpretive capital. No directionality_overrides are authored: the structural derivation from roles, power, and exit produces the right differentiation, and the override surface is keyed by power atom — an override at 'organized' would wrongly homogenize three organized-level seats whose directionalities genuinely differ (two payers, one beneficiary). Suppression is authored unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — report conflict after the Prophet's death — is contested rather than dead: it remains live inside the school (new cases keep arising and the fabrication filter keeps doing work) and is declared superseded by the transmission-hierarchy reading (authenticated reports arbitrate). The classification prevents mislabeling in both directions: this is not a pure snare, because the fabrication-risk filter is a genuine epistemic function that would survive any school-interest analysis; and it is not a pure rope, because the authority asymmetry is structural, actively enforced, and concentrates legitimacy in one lineage. The theater trajectory documents partial mandate atrophy — the living tradition that justified the constraint has largely become a codified canon, so the mandate has drifted while the authority structure persists — but the school's coordinating function in Maghribi legal practice remains live, so the structure is not yet inertial. The mismatch-relevant combination here is founding_problem_status=contested with world_rearranges, which is coherent: the parties dispute the founding problem precisely because the arrangement still rearranges the world if removed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medina_preservation_empirical_status,
    'Is the founding epistemic claim true — did Medina actually preserve prophetic practice more faithfully than Kufa, Basra, or other centers, such that its continuous practice carries independent probative force?',
    'Historical-critical comparison of Medinan ''amal claims against independently authenticated report corpora; analysis of whether Medinan practice tracks or contradicts widely transmitted reports case by case; study of fabrication incentives in Medinan versus Iraqi report transmission.',
    'If the preservation claim fails empirically, the constraint''s coordination justification collapses and the arrangement reclassifies toward snare — school-interest extraction wearing an epistemic cover story. If it holds, the measured extraction is partly a defensible epistemic premium for a genuine filter, supporting the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medina_preservation_empirical_status, empirical, 'Whether the Medinan-preservation premise is historically true or a school-constituting fiction.').

omega_variable(
    amal_content_indeterminacy,
    'What actually constituted ''amal ahl al-Madina at any given time — the jurists'' consensus, market custom, women''s practice, judicial decisions — and who decided?',
    'Codification history: compare the practice claims in the Mudawwana and Malik''s corpus against independent evidence of Medinan custom; trace whether the content of ''amal was observed from the community or set by the juristic class and attributed to it.',
    'If the content of the ''amal was effectively determined by the beneficiary class rather than observed from the community, the constraint is self-referential — the lineage certifies its own authority — and effective extraction rises substantially; if the practice content was independently observable and stable, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_content_indeterminacy, empirical, 'Whether the constraint''s source-content was observed or administered by its beneficiaries.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (refutation literature, appointment networks, school boundary enforcement) or internalized (professional identity fusion that makes methodological exit unthinkable for school-trained jurists), and in what proportion?',
    'Post-exit trajectory: examine jurists who left the school or operated across school lines — if methodological suppression persisted after institutional barriers were removed (e.g., jurists in mixed-jurisdiction courts still unable to treat ''amal as discountable), the internalized component is substantial.',
    'If internalized, effective suppression exceeds the structural measure — the payer seats carry the constraint''s force inside their own training, and enforcement would outlast the institutions that run it, raising the piton-drift risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized enforcement of the practice-priority claim.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (maliki_reading) of the jurisprudential_method_kernel — what would adopting a sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative classification of the sibling stories (hanafi_reading, shafii_reading, hanbali_reading) against this one: the shafii_reading inverts the victim set (hadith specialists become beneficiaries, Medinan practice-claims become the discounted class); the hanafi_reading relocates the beneficiary seat to the reasoning tradition; the disagreement is located at the evidentiary status of continuous communal practice relative to isolated transmitted reports.',
    'The epsilon value authored here is valid only for this reading''s arrangement; a sibling reading is a different constraint with its own epsilon, beneficiaries, and victims, not a measurement parameter of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a four-reading kernel; sibling adoptions shift the victim set and relocate epsilon.').

omega_variable(
    maghribi_function_independence,
    'If the Medinan-preservation claim failed, would the school''s Maghribi institutional function survive on the codified canon alone, or collapse with the claim that grounds it?',
    'Counterfactual institutional analysis: whether Maghribi judicial legitimacy in the corpus period ever rested on anything besides the Medinan chain — local customary law, political appointment, the codified Mudawwana''s own authority.',
    'If the function survives on codified canon alone, the constraint is already drifting toward piton (the living-practice justification is theater over an inertial school structure); if it collapses with the claim, the constraint remains a live tangled_rope whose extraction and coordination stand or fall together.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maghribi_function_independence, conceptual, 'Whether the school''s downstream function is independent of the preservation claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 700, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t700, jurisprudential_method_kernel__maliki_reading, theater_ratio, 700, 0.12).
narrative_ontology:measurement(juri_tr_t760, jurisprudential_method_kernel__maliki_reading, theater_ratio, 760, 0.18).
narrative_ontology:measurement(juri_tr_t820, jurisprudential_method_kernel__maliki_reading, theater_ratio, 820, 0.26).
narrative_ontology:measurement(juri_tr_t880, jurisprudential_method_kernel__maliki_reading, theater_ratio, 880, 0.34).
narrative_ontology:measurement(juri_tr_t940, jurisprudential_method_kernel__maliki_reading, theater_ratio, 940, 0.4).
narrative_ontology:measurement(juri_tr_t1000, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1000, 0.45).

% Extraction over time
narrative_ontology:measurement(juri_be_t700, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 700, 0.32).
narrative_ontology:measurement(juri_be_t760, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 760, 0.4).
narrative_ontology:measurement(juri_be_t820, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 820, 0.48).
narrative_ontology:measurement(juri_be_t880, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 880, 0.54).
narrative_ontology:measurement(juri_be_t940, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 940, 0.57).
narrative_ontology:measurement(juri_be_t1000, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t700, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 700, 0.28).
narrative_ontology:measurement(juri_su_t760, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 760, 0.36).
narrative_ontology:measurement(juri_su_t820, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 820, 0.5).
narrative_ontology:measurement(juri_su_t880, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 880, 0.58).
narrative_ontology:measurement(juri_su_t940, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 940, 0.61).
narrative_ontology:measurement(juri_su_t1000, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1000, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__maliki_reading, 0.08).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Islamic jurisprudential method' covers four structurally distinct constraints — one per reading of the jurisprudential_method_kernel — decomposed per the epsilon-invariance principle. This story authors the maliki_reading: medium epsilon on the Medinan-practice regime, with the Medinan scholarly lineage as beneficiary and non-Medinan interpretive claims as victims. The sibling stories author their own arrangements with shifted victim sets and relocated epsilon (under the shafii_reading the hadith-specialist seat flips from payer to beneficiary and the Medinan practice-claims become the discounted class). Each reading is a separate file with its own stable epsilon; the family is linked through network.affects_constraints, with the upstream epistemic claim (Medina preserved the Prophet's practice) cited as warrant by the downstream school-authority structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
