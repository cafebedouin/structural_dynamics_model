% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Pathologization of Neurodiversity (Neurodiversity Reading)
 *   domain: medical/psychiatric/social
 *
 * SUMMARY:
 *   The Diagnostic and Statistical Manual of Mental Disorders (DSM)
 *   classifies neurodivergent people—those with autism, ADHD, dyslexia, and
 *   related neurological variations—as having psychiatric disorders. The
 *   neurodiversity reading frames this taxonomy as a mechanism that
 *   pathologizes natural human neurological variation in order to enforce
 *   behavioral conformity on institutions that would otherwise have to
 *   accommodate cognitive diversity. The constraint extracts
 *   self-determination from neurodivergent individuals and channeled
 *   institutional and pharmaceutical profits. Its persistence depends on
 *   active suppression of alternative frameworks (neurodiversity model) and
 *   structural exclusion of neurodivergent voices from diagnostic revision
 *   processes. The reading deliberately instantiates high extractiveness and
 *   suppression; the founding problem is dead but the constraint persists as
 *   institutional machinery and pharmaceutical market.
 *
 * KEY AGENTS:
 *   - Neurodivergent individuals: classified as disordered, subject to coercive normalization and denied self-determination in institutional settings (schools, workplaces). Identity-locked exit because neurodivergence is constitutive of cognitive self.
 *   - Children: most vulnerable payer group, trapped in institutions, diagnosed without consent, subject to medication and segregation.
 *   - Institutional conformity enforcers (schools, employers, carceral systems): beneficiaries who outsource conformity enforcement to medical diagnosis rather than redesigning for diversity.
 *   - Pharmaceutical manufacturers: beneficiaries who profit from market expansion tied to DSM category proliferation.
 *   - Diagnostic credential gatekeepers (psychiatrists, psychologists): beneficiaries who gain authority, jurisdiction, and income from DSM system.
 *   - Families of neurodivergent persons: forced to accept pathological label strategically to access accommodations, bearing psychological cost of having a 'disordered' child.
 *   - Neurodiversity self-advocates: observers who see the full structure and contest the pathology frame, but lack institutional power.
 *   - Alternative neurodiversity model proponents: excluded voices structurally suppressed from DSM revision and mainstream psychiatric discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.82).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.76).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Pathologization of Neurodiversity (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical/psychiatric/social").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '27472fb0-66a2-4faa-8e07-7a7e09717d68').
narrative_ontology:cs_kernel_codification('27472fb0-66a2-4faa-8e07-7a7e09717d68', formalized).
narrative_ontology:cs_authority_grounding('27472fb0-66a2-4faa-8e07-7a7e09717d68', extraction).
narrative_ontology:cs_interpretation_layer_present('27472fb0-66a2-4faa-8e07-7a7e09717d68').
narrative_ontology:cs_reading_relation('27472fb0-66a2-4faa-8e07-7a7e09717d68', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('27472fb0-66a2-4faa-8e07-7a7e09717d68', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('27472fb0-66a2-4faa-8e07-7a7e09717d68', foundational, neurodiversity_natural_human_variation).
narrative_ontology:cs_axiom_status(neurodiversity_natural_human_variation, holdable).
narrative_ontology:cs_axiom_grounding('27472fb0-66a2-4faa-8e07-7a7e09717d68', neurodiversity_natural_human_variation, empirically_contingent).
narrative_ontology:cs_axiom('27472fb0-66a2-4faa-8e07-7a7e09717d68', foundational, institutional_conformity_extraction_from_variation).
narrative_ontology:cs_axiom_status(institutional_conformity_extraction_from_variation, holdable).
narrative_ontology:cs_axiom_grounding('27472fb0-66a2-4faa-8e07-7a7e09717d68', institutional_conformity_extraction_from_variation, deontological).
narrative_ontology:cs_reference_frame('27472fb0-66a2-4faa-8e07-7a7e09717d68', neurodiversity_accommodation_framework).
narrative_ontology:cs_drift_state('27472fb0-66a2-4faa-8e07-7a7e09717d68', contemporary_pharmaceutical_market_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('27472fb0-66a2-4faa-8e07-7a7e09717d68', '2026-08-03T14:32:00Z').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_enforcers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, diagnostic_credential_gatekeepers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, children_assigned_pathological_status).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, biomedical_researchers_neurobiology_faction).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, families_of_neurodivergent_persons).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, insurance_and_payment_systems).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, families_of_neurodivergent_persons).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, disability_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classified as disordered by DSM categories (ADHD, autism, dyslexia, etc.) despite their neurological variation being a natural expression of human cognitive diversity. Face coercive normalization pressure through schools, workplaces, and medical systems: medication, behavioral modification programs, exclusion from settings that don't accommodate their needs. Their neurodivergence is constitutive of identity; 'exiting' the DSM framework means accepting the pathological label or social ostracism. The constraint extracts compliance with neurotypical behavioral norms and denies recognition of their neurological self-determination.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, identity_locked, global).

% Subject to diagnostic labeling and institutional interventions (special education segregation, psychotropic medication, behavioral contingency systems) based on DSM categories applied to developmental variation. Have no choice in whether they are diagnosed; their parents and teachers make that choice. Experience the label as a permanent institutional mark that shapes educational placement, peer relationships, and self-concept. Cannot exit because they are minors and institutionally enmeshed.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, children_assigned_pathological_status, payer,
    powerless, immediate, trapped, global).

% Schools, workplaces, military, carceral systems, and other institutions requiring behavioral conformity benefit from the DSM framework by outsourcing conformity enforcement to medical diagnosis. A neurodivergent student who cannot sit still is reframed as 'ADHD disorder' rather than a design failure of the institution. The institution avoids designing environments for cognitive diversity; it medicates or excludes the person instead. The DSM legitimizes institutional rigidity as medical necessity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_enforcers, beneficiary,
    institutional, generational, arbitrage, global).

% Profit directly from DSM category expansion: each new diagnosis creates a market for psychotropic interventions. ADHD diagnoses expanded 5-fold over 30 years; autism spectrum diagnoses expanded similarly. Manufacturers fund DSM revision processes (directly and through professional associations), sponsor diagnostic criteria development, and market treatments as medical solutions to what the reading frames as natural variation. The DSM framework converts diversity into disease markets.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Psychiatrists, psychologists, and allied diagnostic professionals gain authority, legitimacy, and economic rent from the DSM system. Their diagnostic power is what makes the classification binding on schools, workplaces, and insurance systems. Expanding DSM categories expands the diagnostic market and professional jurisdiction. Challenging the DSM framework threatens their epistemic authority and income streams.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, diagnostic_credential_gatekeepers, beneficiary,
    organized, generational, arbitrage, global).

% Research funding, publication venue, and career advancement flow to those investigating neurobiological mechanisms of DSM conditions. The DSM's disease framing generates a research agenda: 'What is broken in the brain of autistic people?' rather than 'How do autistic brains work?' Challenging the pathology premise would redirect research toward neurodiversity mechanisms, potentially reducing funding and prestige for pathology-focused neurobiology.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, biomedical_researchers_neurobiology_faction, beneficiary,
    organized, generational, arbitrage, global).

% Pressured by institutions to seek DSM diagnosis and treatment for their children as the condition of receiving accommodation or avoiding institutional punishment. A parent may accept the diagnosis as a strategic move to access school services ('I don't believe my child has a disorder, but I need the label to get an IEP'). They bear the psychological burden of having a 'disordered' child while also protecting them from coercive normalization. The constraint forces a choice between accepting the pathological label or forgoing institutional support.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, families_of_neurodivergent_persons, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, families_of_neurodivergent_persons, beneficiary).

% Operate in structural contradiction: the DSM pathological label is what triggers legal protections (ADA, IEP rights, workplace accommodations), yet accepting the label reinforces the pathology premise. They work to secure accommodations using DSM diagnoses while simultaneously contesting the DSM framework. Their exit from DSM legitimacy would sacrifice the legal infrastructure protecting neurodivergent people.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, disability_rights_advocates, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, disability_rights_advocates, observer).

% Neuroscientists, clinicians, and advocates promoting a neurodiversity model (neurological variation as natural, not pathological) are systematically excluded from DSM revision processes, have difficulty publishing in mainstream psychiatric journals, and lack funding from institutions invested in the disease model. Their alternative framework is structurally suppressed by institutional gate-keeping tied to the DSM.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, alternative_neurodiversity_model_proponents, excluded,
    moderate, generational, constrained, global).

% Require DSM diagnosis for any mental-health or developmental claim. The DSM is the billing and eligibility infrastructure; it controls what conditions trigger payment. Insurers benefit from the DSM's existence as a neutral-seeming classification system that outsources rationing decisions to 'medical necessity'—a diagnosis either is or isn't in the DSM, so insurers can deny claims outside that boundary.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, insurance_and_payment_systems, beneficiary,
    institutional, generational, arbitrage, global).

% Autistic, ADHD, dyslexic, and other neurodivergent people organized in peer communities rejecting the pathology frame and asserting neurodiversity as identity and culture rather than disorder. They are the analytical seat that sees the full structure—how the DSM extracts self-determination, how institutions benefit from medicalized conformity, how the constraint persists because no single beneficiary group has an incentive to dissolve it. They have minimal institutional power but maximum clarity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_self_advocates, observer,
    powerless, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The DSM provides a standardized diagnostic language that coordinates mental-health practice, research, insurance claims, and educational accommodations across systems. In principle, a shared taxonomy enables consistent case discussion and prevents idiosyncratic labeling.
% TRANSFER_FUNCTION: Transfers self-determination and bodily autonomy from neurodivergent individuals to institutional systems via the pathological classification. Neurodivergent people are required to accept a disease label (or be denied accommodations and face coercive pressure); institutions gain legitimate authority to enforce behavioral conformity; pharmaceutical and diagnostic industries gain a market and professional jurisdiction; schools and employers gain grounds to exclude or medicalize rather than accommodate difference.
% ABSENT_VOICES: Alternative neurodiversity models, neurodivergent self-advocates (historically), non-Western conceptualizations of neurodiversity (many non-Western cultures recognize and accommodate different cognitive styles without pathologization), and the voices of those harmed by diagnostic labeling and forced medication who are not gatekeepers are structurally excluded from DSM revision processes. The processes are controlled by psychiatric professionals and pharmaceutical interests; the people classified are not voting members of the revision committees.
% DISAPPEARANCE_RATIONALE: If the DSM framework vanished overnight, institutional accommodation systems would need redesign—schools would have to accommodate cognitive diversity without diagnostic labels; insurance would need alternative eligibility logic; researchers would shift to studying neurodiversity rather than pathology; pharmaceutical markets for psychotropic treatments of DSM conditions would collapse; diagnostic professionals would lose jurisdiction and income. The absence would force institutional redesign toward accommodation and diversity rather than normalization and exclusion. The world would rearrange fundamentally around the principle that neurological difference is variation, not disease.
% FOUNDING_PROBLEM: Mid-20th-century psychiatry lacked a standardized diagnostic language; clinicians used idiosyncratic terms, making communication and research difficult. The DSM was created to provide a shared taxonomy so psychiatrists could reliably diagnose and discuss cases and conduct comparable research.
% FOUNDING_PROBLEM_CORROBORATION: Historians of psychiatry (Shorter, Kirk, Kutchins) and contemporary DSM critics from within psychiatry (Szasz, Whitaker, Frances, the authors of the Tavistock critique) confirm that the founding problem of diagnostic standardization was solved by DSM-III in 1980. The problem is no longer live as a coordination need; what persists is the institutional infrastructure of the DSM, now expanded into markets and bureaucratic machinery. The psychiatric establishment attests the problem remains live in order to justify ongoing DSM expansion; critics attest the founding problem is solved and the constraint now operates as pure institutional extraction.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) and rising across 50 years because: (1) DSM categories have expanded continuously, classifying more variation as pathology; (2) the market for psychotropic treatments has grown in lockstep with diagnostic expansion; (3) neurodivergent individuals face intensifying pressure to accept pathological labels for access to accommodations; (4) the founding coordination problem (standardized diagnostic language) was solved by DSM-III (1980), but the constraint persists and expands as institutional machinery and pharmaceutical market. Suppression is high (0.76) because: (1) institutional pressure forces acceptance of the diagnosis; (2) alternative frameworks are excluded from diagnostic processes; (3) neurodivergent self-advocates historically had no seat at DSM revision tables; (4) gatekeepers actively suppress neurodiversity framing in psychiatric literature and funding. Theater is moderate-to-high (0.58 at interval end, rising from 0.38) because: (1) the coordination function (standardized diagnostic language) is genuinely real and necessary; (2) but a growing share of DSM activity after 1980 is theatrical—defending category expansions and pharmaceutical markets rather than improving clinical communication; (3) the scientific legitimacy language around DSM revision masks market and jurisdictional interests. Accessibility collapse is moderate (0.71) because: (1) exit appears theoretically possible (reject the diagnosis, live undiagnosed), but in practice institutional pressure closes this off (get diagnosed or lose accommodations and face coercive intervention); (2) identity-lock is the deeper suppression—neurodivergence is constitutive, so 'exiting' means self-rejection. Resistance is substantial (0.64) because: (1) neurodiversity movements, disability rights advocates, critical psychiatry scholars, and peer-support communities actively contest the pathology frame; (2) but their power is dispersed and structurally excluded from DSM governance.
 *
 * PERSPECTIVAL GAP:
 *   The biomedical-reading seat and the neurodiversity-reading seat should compute entirely differently, per the kernel structure. A biomedical psychiatrist sees the DSM as a scientific classification discovering objective disease entities; extractiveness should be near zero (natural kinds, no extraction, mountain-candidate logic). A neurodiversity advocate sees the same DSM framework as institutional machinery extracting conformity and self-determination from neurodivergent people; extractiveness is high (snare logic). The engine will compute different types per seat because directionality is opposite: the biomedical reader is analyst-positioned (analytical power, views DSM as discovery tool, no personal extraction or victimization); the neurodivergent payer is identity-locked (trapped, self-constituted through the trait the DSM pathologizes, forced to accept harm or lose accommodations). The seated divergence is structural, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals are full targets (d near 1.0): identity-locked, powerless, forced to accept pathological labels, denied accommodation unless they accept the diagnosis, experience the constraint as self-harm. Institutional conformity enforcers are beneficiaries (d near 0.0): legitimacy to enforce conformity without redesigning institutions, institutionally powerful, arbitrage-level exit (can shift conformity enforcement strategy if constraint dissolved). Pharmaceutical manufacturers and diagnostic gatekeepers are beneficiaries (d near 0.0): direct profit/jurisdiction/income from constraint, institutional power, arbitrage exits. Families are near-symmetric targets becoming beneficiaries under pressure (d near 0.5 shifting toward 0.3): they experience the pathological label as painful (victim-side pressure) but also use the label strategically to access accommodations (beneficiary-side incentive). The key directionality driver is power + exit + identity-lock: neurodivergent people are powerless and identity-locked (d → 1.0); institutions are powerful with arbitrage exits (d → 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a textbook mandatrophy case: the founding problem (standardized psychiatric diagnostic language for research and clinical communication) was solved by DSM-III in 1980, creating a genuine coordination function at that moment. Since 1980, the DSM has expanded continuously (3 to 5 to 7+ personality disorder subtypes, autism spectrum explosion, ADHD reclassification, new diagnoses added every revision). This expansion is not driven by solving residual coordination problems—it is driven by pharmaceutical marketing (new drugs require diagnoses to sell to) and institutional convenience (schools and employers use diagnosis to exclude rather than accommodate). The constraint persists and intensifies not because participants find it valuable for coordination (they don't—neurodivergent people actively exit if they can, institutions resist accommodation in favor of diagnosis), but because beneficiary interests (pharma, diagnosticians, institutional conformity enforcers) are concentrated and embedded in government, academia, and healthcare infrastructure. The founding mandate—'enable clinical communication through standardized language'—has given way to 'expand pathological categories to serve pharmaceutical and institutional interests.' The theater ratio rising from 0.38 to 0.58 is the signal: an increasing share of DSM activity is theater defending market and jurisdiction rather than improving communication. If neurodivergent people ceased accepting DSM labels, the constraint would dissolve overnight; its persistence is pure institutional inertia and concentrated beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neurodiversity_vs_natural_kind,
    'Is neurodivergence (autism, ADHD, dyslexia, etc.) a natural kind of neurological variation that exists independent of institutional context, or is it constructed relationally—variation that becomes ''disordered'' only when institutions demand conformity?',
    'Cross-cultural empirical study: if non-Western societies with different institutional conformity demands recognize and accommodate the same neurological traits without pathologizing them, the trait is a natural kind; if the ''disorder'' label follows institutional demand for conformity, it is constructed relationally.',
    'If natural kind: the neurodiversity reading is approximately true and the DSM''s pathology framing is imposing institutional values onto natural biology. If constructed relationally: neurodiversity and biomedical readings are both partially true—the trait exists naturally, but the ''disorder'' status is institutional. The constraint would be understood as institutional conformity enforcement, not discovery of natural pathology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodiversity_vs_natural_kind, empirical, 'Whether neurodivergent traits are natural kinds or relationally constructed through institutional context.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of neurodiversity framing and self-advocacy in DSM processes primarily structural (institutional barriers, gatekeeping, funding exclusion) or internalized (neurodivergent people have internalized the pathology frame, neuroscientists believe the biomedical paradigm is objectively true)?',
    'Post-destigmatization trajectory: if neurodiversity framing gains institutional access and funding, does the scientific community shift toward neurodiversity paradigms, or do they defend biomedical frames against evidence? If the latter, suppression is internalized authority-belief rather than external gatekeeping.',
    'If structural: removing gatekeeping (mandating neurodiversity representation on DSM committees) would shift the constraint type toward rope/coordination. If internalized: even with gatekeeping removed, the biomedical paradigm would persist because it is genuinely believed; the constraint would be a Piton (inertial, not extractive). Combined structural + internalized suppression is the current state; mapping the proportion matters for remedy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Suppression mechanism in the exclusion of neurodiversity paradigms from psychiatric authority.').

omega_variable(
    accommodation_vs_medication_institutional_preference,
    'Do institutions prefer medicalized conformity (diagnose, medicate, exclude) over accommodation (redesign environments, support neurodiversity) because medication is genuinely cheaper/more efficient, or because accommodation requires institutional change while medication offloads the problem onto individuals?',
    'Cost accounting: compare lifetime institutional cost of accommodating a neurodivergent student (environmental redesign, support staff, flexible pacing) versus cost of diagnosis, medication, special education segregation, and remediation of exclusion-related harm.',
    'If accommodation is cheaper: the preference for medication is extractive institutional convenience, not efficiency; the constraint is maintainable only by suppressing the economic analysis. If medication is cheaper: part of the extraction is justified by efficiency; the constraint would be Tangled Rope rather than Snare (genuine coordination benefit to institutional efficiency, alongside asymmetric extraction from individuals). Evidence strongly suggests accommodation is cheaper lifetime, making the constraint a pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accommodation_vs_medication_institutional_preference, empirical, 'Comparative institutional cost of accommodation versus medication/exclusion in managing neurodiversity.').

omega_variable(
    kernel_reading_coexistence,
    'Can the biomedical reading and the neurodiversity reading coexist within a single institutional framework (e.g., ''DSM captures real disease entities AND pathologizes natural variation''), or does accepting neurodiversity logically foreclose the biomedical disease framing?',
    'Theoretical analysis: if a trait is a natural human variation (neurodiversity), can it simultaneously be a disease entity (biomedical)? The answer depends on definitions of disease and disorder. Operational resolution: do practitioners actually hold both frames simultaneously (diagnostic disease language + accommodation of diversity), or do they polarize?',
    'If compatible: both readings can coexist; the neurodiversity reading critiques institutional choice to use DSM pathologization rather than diversity accommodation, not the biomedical framework itself. If logically incompatible: this reading forecloses the biomedical reading, which is rare for committed-kernel readings and would require the relationships to shift from coexist_with to forecloses. Current evidence suggests they are operationally incompatible in practice (institutions choose medication + exclusion OR accommodation + diversity framing, not both) even if theoretically compatible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Logical and operational compatibility between biomedical and neurodiversity readings of the DSM taxonomy kernel.').

omega_variable(
    pharmaceutical_causal_link,
    'Does pharmaceutical industry funding and marketing drive DSM category expansion, or does DSM category expansion follow improved neurobiological understanding that pharmaceutical companies then capitalize on?',
    'Historical documentation: examine DSM revision timelines relative to pharmaceutical product launches; analyze pharmaceutical funding flows to DSM committees and advisory boards; study marketing budgets for psychotropic drugs relative to DSM category expansion timeline.',
    'If pharma drives expansion: the constraint is a snare designed as a market for psychotropics, corroborating the critical psychiatry reading. If neurobiological understanding drives expansion, which pharma then markets: the constraint is still extractive but the motive is scientific progress plus market capture (Tangled Rope). Available evidence (Whitaker, Gotzsche, Angell) strongly suggests pharma was a primary driver post-1980, but the question remains open for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_causal_link, empirical, 'Causal direction of pharmaceutical influence on DSM category expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(dsm__tr_t0, observed).
narrative_ontology:measurement(dsm__tr_t8, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement_basis(dsm__tr_t8, observed).
narrative_ontology:measurement(dsm__tr_t16, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement_basis(dsm__tr_t16, observed).
narrative_ontology:measurement(dsm__tr_t24, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 24, 0.53).
narrative_ontology:measurement_basis(dsm__tr_t24, observed).
narrative_ontology:measurement(dsm__tr_t32, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 32, 0.56).
narrative_ontology:measurement_basis(dsm__tr_t32, observed).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(dsm__tr_t40, observed).
narrative_ontology:measurement(dsm__tr_t50, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(dsm__tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(dsm__be_t0, observed).
narrative_ontology:measurement(dsm__be_t8, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(dsm__be_t8, observed).
narrative_ontology:measurement(dsm__be_t16, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement_basis(dsm__be_t16, observed).
narrative_ontology:measurement(dsm__be_t24, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement_basis(dsm__be_t24, observed).
narrative_ontology:measurement(dsm__be_t32, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement_basis(dsm__be_t32, observed).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(dsm__be_t40, observed).
narrative_ontology:measurement(dsm__be_t50, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(dsm__be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(dsm__su_t0, observed).
narrative_ontology:measurement(dsm__su_t8, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(dsm__su_t8, observed).
narrative_ontology:measurement(dsm__su_t16, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(dsm__su_t16, observed).
narrative_ontology:measurement(dsm__su_t24, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement_basis(dsm__su_t24, observed).
narrative_ontology:measurement(dsm__su_t32, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement_basis(dsm__su_t32, observed).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(dsm__su_t40, observed).
narrative_ontology:measurement(dsm__su_t50, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement_basis(dsm__su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.12).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_pharmaceutical_market_coupling).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_enforcement).

% DUAL FORMULATION NOTE:
% The DSM taxonomy kernel has three structurally distinct readings: biomedical (disease discovery), critical psychiatry (pharmaceutical market construction), and neurodiversity (institutional conformity enforcement). Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and types. This file is the neurodiversity reading. The biomedical and critical psychiatry readings are separate constraint stories linked via network.affects_constraints. The three readings coexist because different communities (biomedical psychiatrists, critical scholars, neurodiversity advocates) hold different frameworks; the kernel (the DSM text itself) is the same, but interpretations diverge fundamentally. Beneficiary and victim sets differ sharply across readings: biomedical reading has no identified victims (natural discovery); neurodiversity reading has neurodivergent people as victims; critical psychiatry reading emphasizes pharmaceutical industry beneficiaries. The three readings belong to the same constraint family because they share a kernel and interpretive disagreement; decomposition follows ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, powerless, 0.95).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
