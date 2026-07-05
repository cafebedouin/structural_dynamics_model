% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Reading of the Anthropological/Fossil Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story is one of three readings of a shared kernel: the
 *   anthropological record (fossils, strata, genomes, and material human
 *   origins evidence). This reading holds that the record, correctly
 *   interpreted, is compatible with divine creation event(s) on a timeline
 *   governed by scriptural chronology or by design-inference from biological
 *   complexity. Since the mid-20th century, this reading has hardened from a
 *   loosely held theological position into an institutionally administered
 *   doctrine, complete with credentialing bodies, curriculum publishers, and
 *   museum/research organizations whose funding and legitimacy depend on the
 *   reading's continued authority within affiliated communities. That
 *   institutional hardening — not the underlying theological claim itself —
 *   is what the extraction and suppression metrics in this story track. The
 *   naturalist_reading and indigenous_epistemology_reading constraints are
 *   separate stories with their own ε values and stakeholder structures; this
 *   file does not average across them or describe the contest between them
 *   internally, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - creationist_institutional_leadership: agenda-setter, administers doctrinal conformity
 *   - young_earth_research_organizations: beneficiary, institutional survival tied to reading's authority
 *   - science_educated_congregants: payer, bears social/relational cost of dissent or compartmentalization
 *   - young_earth_dissenting_scientists: payer, career capture under doctrinal employment conditions
 *   - children_in_creationist_schooling: payer, epistemic foreclosure before capacity for independent evaluation
 *   - mainstream_paleontology_and_genetics_community: excluded, evidence source treated as adversary not interlocutor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.52).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.61).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading of the Anthropological/Fossil Record").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, '607dabd5-6412-46dd-bb9c-47d53ca32a50').
narrative_ontology:cs_kernel_codification('607dabd5-6412-46dd-bb9c-47d53ca32a50', fixed_text).
narrative_ontology:cs_authority_grounding('607dabd5-6412-46dd-bb9c-47d53ca32a50', lineage).
narrative_ontology:cs_interpretation_layer_present('607dabd5-6412-46dd-bb9c-47d53ca32a50').
narrative_ontology:cs_reading_relation('607dabd5-6412-46dd-bb9c-47d53ca32a50', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('607dabd5-6412-46dd-bb9c-47d53ca32a50', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('607dabd5-6412-46dd-bb9c-47d53ca32a50', foundational, scriptural_chronology_constrains_material_history).
narrative_ontology:cs_axiom_status(scriptural_chronology_constrains_material_history, holdable).
narrative_ontology:cs_axiom_grounding('607dabd5-6412-46dd-bb9c-47d53ca32a50', scriptural_chronology_constrains_material_history, theological).
narrative_ontology:cs_axiom('607dabd5-6412-46dd-bb9c-47d53ca32a50', secondary, biological_complexity_requires_designer_inference).
narrative_ontology:cs_axiom_status(biological_complexity_requires_designer_inference, holdable).
narrative_ontology:cs_axiom_grounding('607dabd5-6412-46dd-bb9c-47d53ca32a50', biological_complexity_requires_designer_inference, empirically_contingent).
narrative_ontology:cs_reference_frame('607dabd5-6412-46dd-bb9c-47d53ca32a50', scriptural_chronological_inerrancy).
narrative_ontology:cs_drift_state('607dabd5-6412-46dd-bb9c-47d53ca32a50', post_genomic_dating_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('607dabd5-6412-46dd-bb9c-47d53ca32a50', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_institutional_leadership).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, young_earth_research_organizations).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, affiliated_curriculum_publishers).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, science_educated_congregants).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, young_earth_dissenting_scientists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, children_in_creationist_schooling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets doctrinal boundaries on how the fossil and genetic record may be interpreted within affiliated congregations, schools, and publishing houses. Administers statements of faith that credential-check teachers and scientists for continued employment or platform. Collects tithing, enrollment, and conference revenue tied to maintaining the reading's authority.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Produce museum exhibits, journals, and public materials arguing the geological and fossil record fits a young-earth timeline. Funded by donations and ticket sales that depend on the reading remaining the trusted default among a donor base; institutional survival is tied to defending the interpretation against mainstream paleontology and genetics.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, young_earth_research_organizations, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, young_earth_research_organizations, agenda_setter).

% Sell textbooks and homeschool curricula built around the creationist reading. Revenue depends on schools and parents continuing to prefer materials that harmonize the record with the scriptural timeline over materials teaching the naturalist consensus.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, affiliated_curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Encounter mainstream geology, genetics, or paleontology through education or work and must either compartmentalize what they've learned, quietly leave the community, or publicly dissent at real social and relational cost. Exit means risking family and social standing built around shared religious identity.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, science_educated_congregants, payer,
    moderate, biographical, constrained, local).

% Trained scientists within creationist-affiliated institutions who privately doubt the young-earth timeline but face termination, loss of credential-granting affiliation, or reputational exile within their professional community if they say so publicly. Their labor (research, teaching) is captured by an institution that requires doctrinal conformity as a condition of employment.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, young_earth_dissenting_scientists, payer,
    moderate, biographical, trapped, national).

% Are taught the creationist reading as settled fact before they have the tools to independently evaluate competing interpretations of the record. Their later ability to engage mainstream science, pursue certain careers, or hold credibility outside their community is shaped by this early foreclosure; they did not choose the curriculum.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, children_in_creationist_schooling, payer,
    powerless, biographical, trapped, local).

% Produces the dated stratigraphic, radiometric, and genomic evidence the creationist reading must reinterpret or reject. Has no voice within creationist institutional governance and is typically addressed only as an adversary to be rebutted, not a source consulted on equal footing.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, mainstream_paleontology_and_genetics_community, excluded,
    institutional, generational, analytical, global).

% Hold rival readings of the same anthropological record (relational-ancestral continuity, or materialist evolutionary origin) that are treated by creationist institutions as either mistaken or spiritually hostile, rather than as competing epistemic frameworks warranting engagement.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, indigenous_and_naturalist_reading_communities, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable interpretive framework that lets a religious community read the physical record (fossils, strata, genomes) as consistent with its scriptural cosmology, preserving doctrinal and community coherence across generations without requiring members to adjudicate technical geology or genetics themselves.
% TRANSFER_FUNCTION: Moves donation revenue, tithing, curriculum purchases, and conference/ticket income toward institutions and publishers who administer the reading; moves career risk, social standing, and unexamined educational foreclosure onto dissenting scientists, science-literate congregants, and children raised inside the framework.
% ABSENT_VOICES: Mainstream paleontologists and geneticists whose evidence must be reinterpreted are not present in doctrinal governance. Indigenous epistemology holders and naturalist-reading scientists who hold rival accounts of the same record are treated as external opponents rather than parties with standing in the interpretive dispute.
% DISAPPEARANCE_RATIONALE: From the leadership and research-organization seats, the reading's disappearance would mean loss of doctrinal coherence, donor base, and institutional identity — the world clearly rearranges for them. From an outside observer's seat, the underlying anthropological record (strata, fossils, genomes) is unaffected either way; only the interpretive apparatus built atop it would vanish, and adherents would simply adopt one of the sibling readings or a moderated position. The parties dispute which description is true.
% FOUNDING_PROBLEM: Reconciling a community's scriptural commitments with an accumulating body of physical evidence (geological strata, fossil succession, radiometric dating, comparative genomics) that appeared to describe deep time and common descent, in a way that let the community continue to treat scripture as historically and scientifically authoritative.
% FOUNDING_PROBLEM_CORROBORATION: Creationist institutional leadership and affiliated research organizations attest the problem is live and unresolved (the evidence, in their account, remains genuinely ambiguous or is being actively misread by secular science). Historians of science and sociologists of religion outside the movement, along with the mainstream paleontology and genetics community, attest that the empirical dating and phylogenetic questions were substantially settled decades ago by convergent independent methods, and that the reading now persists chiefly as an identity and institutional-authority mechanism rather than a live unresolved evidentiary dispute.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, contested).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) and suppression (0.61) are moderate-to-substantial and rising over the interval, reflecting a shift from the reading functioning primarily as shared theological coordination toward functioning increasingly as an institutionally enforced boundary condition on employment, curriculum adoption, and social belonging. Theater ratio (0.44) is meaningfully elevated because much of the visible activity — rebuttal literature, exhibit displays, apologetics conferences — increasingly performs engagement with the scientific record for an already-persuaded audience rather than genuinely adjudicating open empirical questions. Accessibility collapse (0.58) and resistance (0.55) are mid-range: alternative readings are known to exist and are not eliminated from the wider culture, but within the institutional core the range of expressible positions has narrowed, and dissent meets real, organized resistance from leadership.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, the reading is lived as a coordination mechanism preserving doctrinal, communal, and pedagogical coherence — a genuine solution to the problem of relating scripture to physical evidence. From the payer seats — dissenting scientists under employment threat, congregants managing cognitive dissonance, children given no alternative framework — the same structure operates as an enforced boundary that forecloses inquiry and imposes real costs for stepping outside it. The engine computes these divergent seat-level classifications from the structural power/exit data; the claimed_type (tangled_rope) is authored independently of any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership and affiliated organizations sit near the beneficiary end: they administer the boundary and derive revenue, credibility, and institutional continuity from its maintenance, and hold arbitrage-level exit (they can moderate or double down as strategically useful). Dissenting scientists and science-educated congregants sit near the target end: their exit is constrained or trapped by career and relational lock-in, and the reading's persistence imposes direct costs on them. Children in creationist schooling are the most extreme target case — power is powerless, exit is trapped by definition (they did not choose enrollment), and the foreclosure of alternative frameworks happens before they can meaningfully consent or dissent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling scripture with an evidentiary record that seemed to describe deep time — was a genuine intellectual and pastoral problem when first confronted. Whether it remains live is exactly the contested question the six_questions genealogy surfaces: leadership attests it is still live and unresolved, while outside historians of science and the mainstream evidentiary community attest the empirical questions were substantially settled independently decades ago, and that the reading's institutional apparatus (credentialing, curriculum sales, museum revenue) now persists partly because dismantling it would cost the administering institutions their funding base and authority — not because the founding evidentiary dispute remains open. This is the tangled_rope signature: real coordination function (community coherence, shared meaning-making) coexisting with asymmetric extraction (captured labor, foreclosed children, socially costly dissent) sustained by active enforcement (doctrinal statements, employment conditions, credentialing gates).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading (creationist_reading) of the anthropological_record kernel, alongside naturalist_reading and indigenous_epistemology_reading. Where exactly is the disagreement located: is it about what the physical record shows, about what counts as valid method for reading it, or about which authority (scriptural text, scientific consensus, oral tradition) gets final adjudicative say?',
    'Compare the three readings'' cs_structure.authority_grounding and axioms directly: naturalist_reading grounds authority in expertise/scientific method, indigenous_epistemology_reading grounds it in practice/oral tradition, this reading grounds it in lineage/scriptural text. The disagreement is not primarily about the raw stratigraphic or genomic data (which all three readings can in principle observe) but about which adjudicating authority is permitted to settle contested interpretations of that data.',
    'If the disagreement is located at the authority layer rather than the data layer, then no amount of additional fossil or genomic evidence resolves the kernel contest — only a shift in which authority a given community defers to would change the reading in force for that community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locates the kernel disagreement at the authority-grounding layer, not the raw-evidence layer.').

omega_variable(
    naturality_vs_institutional_construction,
    'Is the creationist reading, at its theological core, a natural feature of sincere religious commitment reasoning honestly under genuine evidentiary ambiguity — or has the institutional apparatus built around it (credentialing bodies, curriculum publishers, research organizations) become a self-perpetuating structure whose survival now depends on maintaining the reading regardless of evidentiary developments?',
    'Track whether affiliated institutions have historically revised specific factual claims (e.g., flood geology details, specific dating claims) in response to internal or external evidentiary pressure, versus whether institutional employment and funding conditions have hardened around fixed conclusions over the same period. A pattern of doctrinal statements becoming more rather than less restrictive over time would indicate institutional self-perpetuation outpacing genuine theological reasoning.',
    'If institutional self-perpetuation dominates, the tangled_rope classification is conservative and the extraction/suppression trajectory should be read as understating true institutional capture; if genuine theological reasoning dominates and institutions are secondary administrative structures, the coordination function is stronger than the extraction function suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_institutional_construction, empirical, 'Whether the reading is primarily sincere theological reasoning or institutionally self-perpetuating doctrine.').

omega_variable(
    childhood_epistemic_foreclosure_severity,
    'How severe and how reversible is the epistemic foreclosure experienced by children raised exclusively within creationist schooling, relative to children raised within any other single-framework educational environment (including strictly naturalist or strictly indigenous-epistemology schooling)?',
    'Longitudinal studies of adults who exited creationist-only education, comparing their later capacity to engage mainstream scientific literature and their reported sense of having been denied alternative frameworks, against comparable studies of adults exiting other single-framework educational environments.',
    'If foreclosure severity is comparable across all single-framework educational environments regardless of content, the harm to children_in_creationist_schooling is a general feature of monopolistic childhood education rather than something specific to the creationist content — which would narrow (but not eliminate) the victim-specific extraction claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(childhood_epistemic_foreclosure_severity, empirical, 'Whether childhood epistemic foreclosure is content-specific to this reading or general to single-framework education.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(anth_tr_t0, observed).
narrative_ontology:measurement(anth_tr_t12, anthropological_record__creationist_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(anth_tr_t12, observed).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__creationist_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(anth_tr_t24, observed).
narrative_ontology:measurement(anth_tr_t36, anthropological_record__creationist_reading, theater_ratio, 36, 0.37).
narrative_ontology:measurement_basis(anth_tr_t36, observed).
narrative_ontology:measurement(anth_tr_t48, anthropological_record__creationist_reading, theater_ratio, 48, 0.41).
narrative_ontology:measurement_basis(anth_tr_t48, observed).
narrative_ontology:measurement(anth_tr_t60, anthropological_record__creationist_reading, theater_ratio, 60, 0.44).
narrative_ontology:measurement_basis(anth_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(anth_be_t0, observed).
narrative_ontology:measurement(anth_be_t12, anthropological_record__creationist_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(anth_be_t12, observed).
narrative_ontology:measurement(anth_be_t24, anthropological_record__creationist_reading, base_extractiveness, 24, 0.43).
narrative_ontology:measurement_basis(anth_be_t24, observed).
narrative_ontology:measurement(anth_be_t36, anthropological_record__creationist_reading, base_extractiveness, 36, 0.47).
narrative_ontology:measurement_basis(anth_be_t36, observed).
narrative_ontology:measurement(anth_be_t48, anthropological_record__creationist_reading, base_extractiveness, 48, 0.5).
narrative_ontology:measurement_basis(anth_be_t48, observed).
narrative_ontology:measurement(anth_be_t60, anthropological_record__creationist_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(anth_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(anth_su_t0, observed).
narrative_ontology:measurement(anth_su_t12, anthropological_record__creationist_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(anth_su_t12, observed).
narrative_ontology:measurement(anth_su_t24, anthropological_record__creationist_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement_basis(anth_su_t24, observed).
narrative_ontology:measurement(anth_su_t36, anthropological_record__creationist_reading, suppression_requirement, 36, 0.55).
narrative_ontology:measurement_basis(anth_su_t36, observed).
narrative_ontology:measurement(anth_su_t48, anthropological_record__creationist_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement_basis(anth_su_t48, observed).
narrative_ontology:measurement(anth_su_t60, anthropological_record__creationist_reading, suppression_requirement, 60, 0.61).
narrative_ontology:measurement_basis(anth_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__creationist_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the anthropological_record kernel (naturalist_reading, indigenous_epistemology_reading, creationist_reading). All three are linked bidirectionally in the network graph per the ε-invariance decomposition principle: they share an evidentiary substrate but diverge on adjudicating authority, and each carries its own independently authored ε, beneficiary/victim structure, and classification. This file's stakeholders, metrics, and six_questions describe only the creationist_reading structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
