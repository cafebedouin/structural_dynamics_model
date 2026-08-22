% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__literary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__literary_framework, []).

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
 *   constraint_id: genesis_creation_cosmology__literary_framework
 *   human_readable: Literary-Framework Reading of Genesis 1-2 (ANE Cosmological Schema as Scaffolding)
 *   domain: religious_studies/theology/philosophy_of_science
 *
 * SUMMARY:
 *   The literary-framework reading governs how Genesis 1-2 may legitimately
 *   be used: the chapters' cosmological vocabulary — tiered cosmos, solid
 *   firmament, waters above, functional assignment of the creation days — is
 *   assigned to Ancient Near Eastern genre convention and rhetorical
 *   architecture, and the text is held to make no cosmological claims at all.
 *   Instituted through comparative philology (Gunkel onward) and consolidated
 *   in mainline seminaries across the twentieth century, the arrangement
 *   removes the creation chapters from scientific adjudication and from
 *   doctrinal proof-texting alike, transferring custody of the text's meaning
 *   to credentialed historical-critical scholarship. Both displaced
 *   authorities experience the transfer differently: scientific institutions
 *   gain a quieter classroom, traditional doctrinal offices lose a warrant,
 *   and the text itself is reclassified from normative constraint to cultural
 *   artifact. Time points index years since 1925. Claim and metrics are
 *   authored independently: claimed_type records my structural judgment
 *   (tangled_rope — genuine coordination with asymmetric transfer and active
 *   enforcement); the metrics record the arrangement's operation as I assess
 *   it descriptively.
 *
 * KEY AGENTS:
 *   - biblical_studies_academy: agenda-setting custodian (institutional/constrained) — produces and polices the framework reading through peer review, commentaries, and curricula; collects interpretive authority
 *   - mainline_denominations: dual-positioned beneficiary-payer (organized/constrained) — purchases coexistence with science, pays in retired doctrinal warrants and mediated interpretation
 *   - science_education_establishment: incidental beneficiary (organized/mobile) — receives a quieter classroom without administering the arrangement
 *   - conservative_literalist_communities: primary target (organized/identity_locked) — plain-sense reading reclassified as category error; contests indefinitely through parallel institutions
 *   - systematic_theologians: secondary target with offsetting gain (organized/constrained) — loses Genesis as doctrinal warrant, gains release from untenable defenses
 *   - lay_devotional_readers: diffuse payer-beneficiary (moderate/mobile) — mediated access to their own scripture exchanged for freedom from the science-faith accusation
 *   - young_earth_creationist_scholars: excluded voice (organized/trapped) — technical dissent ruled out of bounds before argument begins in adjudicating venues
 *   - philosophers_of_science_and_religion: analytical observer (analytical/analytical) — examines how the reading allocates questions among science, doctrine, and history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__literary_framework, 0.38).
domain_priors:suppression_score(genesis_creation_cosmology__literary_framework, 0.34).
domain_priors:theater_ratio(genesis_creation_cosmology__literary_framework, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, extractiveness, 0.38).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__literary_framework, tangled_rope).
narrative_ontology:human_readable(genesis_creation_cosmology__literary_framework, "Literary-Framework Reading of Genesis 1-2 (ANE Cosmological Schema as Scaffolding)").
narrative_ontology:topic_domain(genesis_creation_cosmology__literary_framework, "religious_studies/theology/philosophy_of_science").

domain_priors:requires_active_enforcement(genesis_creation_cosmology__literary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__literary_framework, 'e8e3a052-8555-4c85-9224-5457125a5a04').
narrative_ontology:cs_kernel_codification('e8e3a052-8555-4c85-9224-5457125a5a04', fixed_text).
narrative_ontology:cs_authority_grounding('e8e3a052-8555-4c85-9224-5457125a5a04', expertise).
narrative_ontology:cs_interpretation_layer_present('e8e3a052-8555-4c85-9224-5457125a5a04').
narrative_ontology:cs_reading_relation('e8e3a052-8555-4c85-9224-5457125a5a04', genesis_creation_cosmology__young_earth_literal, forecloses).
narrative_ontology:cs_reading_relation('e8e3a052-8555-4c85-9224-5457125a5a04', genesis_creation_cosmology__theistic_evolution, influences).
narrative_ontology:cs_axiom('e8e3a052-8555-4c85-9224-5457125a5a04', foundational, genesis_makes_no_cosmological_claims).
narrative_ontology:cs_axiom_status(genesis_makes_no_cosmological_claims, holdable).
narrative_ontology:cs_axiom_grounding('e8e3a052-8555-4c85-9224-5457125a5a04', genesis_makes_no_cosmological_claims, empirically_contingent).
narrative_ontology:cs_axiom('e8e3a052-8555-4c85-9224-5457125a5a04', foundational, scripture_custody_belongs_to_historical_criticism).
narrative_ontology:cs_axiom_status(scripture_custody_belongs_to_historical_criticism, holdable).
narrative_ontology:cs_axiom_grounding('e8e3a052-8555-4c85-9224-5457125a5a04', scripture_custody_belongs_to_historical_criticism, conventional).
narrative_ontology:cs_axiom('e8e3a052-8555-4c85-9224-5457125a5a04', secondary, comparative_genre_evidence_adjudicates_reference).
narrative_ontology:cs_axiom_status(comparative_genre_evidence_adjudicates_reference, holdable).
narrative_ontology:cs_axiom_grounding('e8e3a052-8555-4c85-9224-5457125a5a04', comparative_genre_evidence_adjudicates_reference, empirically_contingent).
narrative_ontology:cs_reference_frame('e8e3a052-8555-4c85-9224-5457125a5a04', ancient_near_eastern_cultural_artifact).
narrative_ontology:cs_drift_state('e8e3a052-8555-4c85-9224-5457125a5a04', contemporary_postcritical_theology, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e8e3a052-8555-4c85-9224-5457125a5a04', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__literary_framework, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, biblical_studies_academy).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, mainline_denominations).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, science_education_establishment).
narrative_ontology:constraint_victim(genesis_creation_cosmology__literary_framework, conservative_literalist_communities).
narrative_ontology:constraint_victim(genesis_creation_cosmology__literary_framework, systematic_theologians).
narrative_ontology:constraint_victim(genesis_creation_cosmology__literary_framework, lay_devotional_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, systematic_theologians).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, lay_devotional_readers).
narrative_ontology:constraint_victim(genesis_creation_cosmology__literary_framework, mainline_denominations).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__literary_framework, genre_relative_reference_doctrine).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__literary_framework, comparative_method_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University departments, seminary faculties, and learned societies that produce the commentaries, curricula, and peer-reviewed literature in which the framework reading is stated, taught, and adjudicated. They determine which interpretations circulate as scholarship and which are returned to authors as category mistakes. Professional standing, hiring, and publication depend on the comparative-anthropological method the reading rests on; leaving the field would forfeit accumulated specialization.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, biblical_studies_academy, agenda_setter,
    institutional, generational, constrained, global).

% Denominations that adopted the framework reading to retain educated members and coexist with public science education. They receive relief from a conflict that was splitting congregations and costing credibility, and they pay for it by routing official interpretation through academically credentialed clergy and by retiring long-standing doctrinal uses of the creation chapters. Member expectations, property, and liturgical continuity make wholesale exit from the arrangement costly.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, mainline_denominations, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__literary_framework, mainline_denominations, payer).

% Teachers, curriculum boards, and science-communication institutions for whom the framework reading removes Genesis from the pool of texts that can be mobilized against evolutionary biology in classrooms and textbooks. They gain a quieter classroom; they neither administer nor fund the interpretive arrangement and can relocate or reframe their work at little cost.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, science_education_establishment, beneficiary,
    organized, biographical, mobile, national).

% Churches, parachurch organizations, and lay networks for whom the plain-sense, sequential reading of Genesis 1-2 is constitutive of scriptural trustworthiness as such. Under the framework reading their reading is reclassified as a pre-critical category mistake rather than engaged as a rival interpretation. They respond by building parallel schools, publishers, museums, and media; abandoning the plain-sense reading would unravel commitments binding their communities together, so they do not exit the dispute — they contest it indefinitely.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, conservative_literalist_communities, payer,
    organized, generational, identity_locked, global).

% Doctrine-writers in confessional traditions who lose Genesis 1-2 as a warrant for claims about cosmic order, human uniqueness, and divine action, and must rebuild those doctrines on other grounds. They gain release from defending positions that natural science had made untenable. Their vocation ties them to the traditions whose interpretive habits the framework reading overrides.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, systematic_theologians, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__literary_framework, systematic_theologians, beneficiary).

% Ordinary members of religious communities who read Genesis devotionally and are told, when they inquire, that the surface sense is not the point. They bear a diffuse cost — mediated access to their own scripture — and a diffuse benefit — freedom from the accusation that faith requires rejecting science. Switching congregations or traditions is comparatively easy for them, which blunts whatever pressure the arrangement puts on them individually.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, lay_devotional_readers, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__literary_framework, lay_devotional_readers, beneficiary).

% Researchers with technical training who argue for a young earth and a literal creation week. Within the journals, societies, and faculties where the framework reading is adjudicated, their conclusions are set aside before argument begins, as flowing from a disqualifying prior rather than engaging the evidence. They publish through their own institutions instead; entry into the adjudicating venues is effectively closed to them.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, young_earth_creationist_scholars, excluded,
    organized, generational, trapped, national).

% Scholars who examine how the framework reading allocates questions between science, doctrine, and history — what it counts as a cosmological claim, what it counts as genre, and on what authority those counts rest. They take no side in the dispute over the text's meaning and bear none of its costs.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, philosophers_of_science_and_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_cosmology__literary_framework, biblical_studies_academy).
narrative_ontology:fixing_cost_class(genesis_creation_cosmology__literary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the creation chapters out of empirical adjudication and into genre-and-history, giving scientific institutions, mainline religious bodies, and academic biblical scholarship a shared protocol that lets them coexist without each governing the others' claims.
% TRANSFER_FUNCTION: Moves interpretive authority over Genesis 1-2 from doctrinal offices and unmediated lay reading to credentialed historical-critical specialists, and moves the text itself out of the currency of scientific and apologetic argument into the currency of cultural-historical analysis.
% ABSENT_VOICES: Young-earth and concordist researchers are structurally absent from the venues where the reading is adjudicated — their objections are classified in advance as flowing from a disqualifying prior rather than engaged as rival interpretations. Lay readers without seminary training appear as objects of pedagogy rather than as interpreting parties. Both groups exist in large numbers outside the room.
% DISAPPEARANCE_RATIONALE: If the framework convention vanished overnight, the science-faith conflict over Genesis would reignite along its pre-consensus lines: mainline denominations would face renewed pressure to choose between scientific literacy and textual plain sense, seminaries would lose the reconciliation curriculum that organizes their Old Testament teaching, and the creation chapters would return to circulation as evidence in biological and cosmological disputes. The arrangement's beneficiaries are numerous and organized enough that its removal rearranges rather than settles anything.
% FOUNDING_PROBLEM: The post-Darwin collision between a plain-sense reading of Genesis 1-2 and modern geology and evolutionary cosmology, which threatened the credibility of religious institutions, the peace of congregations split over the issue, and the functioning of public education.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the history of the controversy itself — legislative battles over evolution instruction, denominational schisms of the 1920s, and the continuing institution-building of creationist movements — and by historians of science whose accounts of the post-Darwin period do not depend on any party's advocacy. Fully neutral attestation is scarce: nearly every witness to the founding problem is also a combatant in it, which is itself signal about how the problem is framed.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__literary_framework, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__literary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__literary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_cosmology__literary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_cosmology__literary_framework, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__literary_framework_tests).
:- end_tests(genesis_creation_cosmology__literary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.38: the arrangement performs a real jurisdictional transfer — custody of the text's meaning moves to credentialed specialists, doctrinal offices lose a warrant, lay readers gain mediated access — but a large share of its operation is genuine conflict resolution whose benefits flow broadly, keeping it well below snare-range extraction. Suppression 0.34, authored as a raw structural property (only extractiveness is scaled by directionality and scope in the engine's computation): enforcement is real but localized to credentialing venues — journal review, faculty hiring, ordination standards — while full alternatives persist in parallel institutions, so it sits far below coercive-monopoly levels. Theater_ratio 0.24: the comparative philology is functional, but a growing share of curricular and homiletic activity ritually restates the framework conclusion ('contextualization') rather than extending it. Accessibility_collapse 0.25: understanding the framework reading does not collapse the literal alternative, which remains fully coherent within its own epistemology — nowhere near natural-law closure. Resistance 0.58: a century of organized counter-institution-building (schools, publishers, museums, litigation) documents sustained active resistance. The three measurement series share one time grid (points 0, 20, 40, 60, 80, 100). Suppression_requirement is tracked because the story's enforcement picture genuinely changes: enforcement capacity was built up across the twentieth century (ordination standards, curricular consolidation) and then plateaued as literalism retreated to parallel institutions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat and the payer seats compute different types from identical structural data. From the academy's position the arrangement is descriptive scholarship it conducts, not a constraint it imposes — extraction reads as near zero and the structure presents as coordination. From the literalist seat the same structure operates as enforced delegitimation: their reading is not refuted but disqualified, and the enforcement is visible in every closed venue. Mainline denominations occupy both seats at once — they experience the arrangement as a peace treaty they signed and as a surrender of interpretive sovereignty they resent. The engine computes this divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the academy (d near 0.05-0.1) collects custody, careers, and curricular control; the science-education establishment (d near 0.1) receives classroom quiet without administering anything. Victim declarations drive high directionality: literalist communities (d near 0.85-0.9) bear delegitimation with identity-locked exit, sitting near the full-target end; systematic theologians (d near 0.55-0.6) bear lost warrants partially offset by released burdens. Dual-positioned seats sit mid-scale: mainline denominations (d near 0.3-0.35) collect peace and pay autonomy; lay readers (d near 0.45-0.5) bear diffuse mediated-access costs with mobile exit damping effective extraction. No directionality_overrides are authored: override granularity is the power atom, and four distinct seats share the 'organized' atom, so any override would misfire across seats the derivation already separates correctly through role and exit data; dual positioning is carried by secondary_role instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the post-Darwin collision between plain-sense Genesis and geological/evolutionary cosmology — remains live wherever literalist communities meet public science education, so the mandate has not outlived its function and mandatrophy_resolved is not declared; no sunset clause exists. The tangled-rope classification prevents two opposite mislabels: reading the arrangement as a snare would erase the genuine, broadly distributed coordination benefit (defused conflict, preserved scholarly study of the text) that explains why non-captured parties voluntarily maintain it; reading it as a pure rope would erase the documented asymmetry — concentrated custody accruing to the academy, enforced exclusion of concordist scholarship, displaced traditional authority — that explains why the losers do not consent. The receipt surface records the asymmetry: gains accrue to a named seat (the academy), and fixing is prohibitive for whoever could fix it, since abandoning the framework convention would reopen the very conflict the participating institutions exist to manage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story instantiates one reading (literary_framework) of the kernel genesis_creation_cosmology; the young_earth_literal and theistic_evolution readings are separate constraints with their own epsilon, beneficiaries, and victims. Which structural elements of the kernel do the readings actually disagree about?',
    'Cross-reading comparison at the engine layer: align the three stories'' victim sets, epsilon values, and computed types; locate the disagreement in whether the text carries cosmological or normative content at all.',
    'If the readings disagreed only on evaluation they would be one constraint with an observer-axis dispute; as authored they disagree on victim sets and epsilon, so they are genuinely distinct constraints and cross-reading type divergence is signal rather than noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the text''s claim-content.').

omega_variable(
    descriptive_vs_accommodative_origin,
    'Is the literary-framework reading a description recovered from the text''s ancient communicative setting, or an accommodation constructed after Darwin to protect institutional credibility?',
    'Comparative philology: whether ANE genre conventions independently establish non-referential cosmological framing, evidenced without reference to modern scientific pressure; dating of interpretive shifts against scientific developments.',
    'If accommodative, the coordination function partly covers institutional self-protection and effective extraction rises toward the snare boundary; if descriptive, the coordination function is genuine and the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(descriptive_vs_accommodative_origin, empirical, 'Naturalness ambiguity: discovered genre fact versus retrofitted harmonization.').

omega_variable(
    literalist_harm_status,
    'Are conservative literalist communities genuinely bearing costs under this arrangement (delegitimation, exclusion from venues), or merely contradicted within ordinary interpretive pluralism?',
    'Audit exclusion events: journal rejection patterns, faculty and ordination credentialing standards, funding and platform access, contrasted with mere published disagreement.',
    'Concentrated, enforced exclusion supports the asymmetric-extraction half of the tangled-rope structure; diffuse disagreement alone would push the arrangement toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literalist_harm_status, empirical, 'Whether the payer seat bears extraction or ordinary contestation.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the measured suppression among affected believers structural (credentialing gatekeeping, venue closure) or internalized (self-censorship of plain-sense readings, anticipatory deference to academic consensus)?',
    'Post-exit trajectory: survey and interview believers who left mainline traditions for independent congregations — if deference to critical consensus persists after leaving the enforcing institutions, part of the suppression is internalized.',
    'Internalized suppression extends the constraint''s effective reach beyond its enforcement infrastructure and would raise the suppression component of per-seat classifications for lay seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized suppression mechanism in interpretive deference.').

omega_variable(
    artifact_status_valence,
    'Is the displacement of the text from normative constraint to cultural artifact a harm (to communities constituted by its normativity) or a benefit (to intellectual honesty and peaceful coexistence) — and can that be settled structurally at all?',
    'Not resolvable by data alone: it depends on whether scriptural normativity is treated as a good to be preserved or a claim to be tested; longitudinal outcomes (retention, conflict rates, doctrinal vitality) inform but do not settle it.',
    'If normativity-loss is counted as harm, the payer side of the ledger grows and extraction estimates rise; if counted as liberation, the same facts read as subsidy. The classification''s stability across this choice is itself diagnostic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(artifact_status_valence, preference, 'Preference-class ambiguity in evaluating the artifact-status outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__literary_framework, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_cosmology__literary_framework, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_cosmology__literary_framework, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(gene_tr_t20, observed).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_cosmology__literary_framework, theater_ratio, 40, 0.14).
narrative_ontology:measurement_basis(gene_tr_t40, observed).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_cosmology__literary_framework, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(gene_tr_t60, observed).
narrative_ontology:measurement(gene_tr_t80, genesis_creation_cosmology__literary_framework, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(gene_tr_t80, observed).
narrative_ontology:measurement(gene_tr_t100, genesis_creation_cosmology__literary_framework, theater_ratio, 100, 0.24).
narrative_ontology:measurement_basis(gene_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_cosmology__literary_framework, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t20, genesis_creation_cosmology__literary_framework, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(gene_be_t20, observed).
narrative_ontology:measurement(gene_be_t40, genesis_creation_cosmology__literary_framework, base_extractiveness, 40, 0.24).
narrative_ontology:measurement_basis(gene_be_t40, observed).
narrative_ontology:measurement(gene_be_t60, genesis_creation_cosmology__literary_framework, base_extractiveness, 60, 0.29).
narrative_ontology:measurement_basis(gene_be_t60, observed).
narrative_ontology:measurement(gene_be_t80, genesis_creation_cosmology__literary_framework, base_extractiveness, 80, 0.34).
narrative_ontology:measurement_basis(gene_be_t80, observed).
narrative_ontology:measurement(gene_be_t100, genesis_creation_cosmology__literary_framework, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(gene_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_cosmology__literary_framework, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t20, genesis_creation_cosmology__literary_framework, suppression_requirement, 20, 0.17).
narrative_ontology:measurement_basis(gene_su_t20, observed).
narrative_ontology:measurement(gene_su_t40, genesis_creation_cosmology__literary_framework, suppression_requirement, 40, 0.24).
narrative_ontology:measurement_basis(gene_su_t40, observed).
narrative_ontology:measurement(gene_su_t60, genesis_creation_cosmology__literary_framework, suppression_requirement, 60, 0.3).
narrative_ontology:measurement_basis(gene_su_t60, observed).
narrative_ontology:measurement(gene_su_t80, genesis_creation_cosmology__literary_framework, suppression_requirement, 80, 0.33).
narrative_ontology:measurement_basis(gene_su_t80, observed).
narrative_ontology:measurement(gene_su_t100, genesis_creation_cosmology__literary_framework, suppression_requirement, 100, 0.34).
narrative_ontology:measurement_basis(gene_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__literary_framework, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_cosmology__literary_framework, genesis_creation_cosmology__young_earth_literal).
narrative_ontology:affects_constraint(genesis_creation_cosmology__literary_framework, genesis_creation_cosmology__theistic_evolution).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: colloquial references to 'what Genesis says about creation' bundle three structurally distinct claims — a literal chronological claim (young_earth_literal), a compatibilist theological-truth claim (theistic_evolution), and a no-claim literary-framework claim (this story). Each carries its own epsilon, beneficiary/victim structure, and enforcement profile; measuring them through one label would produce observer-dependent epsilon and defeat classification. The upstream/downstream ordering runs from this reading outward: the framework reading supplies the genre analysis that theistic_evolution presupposes and that young_earth_literal rejects, so its fortunes shift the operating conditions of both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
