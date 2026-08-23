% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Comprehensive Derivative Work Enclosure Reading
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   The enclosure reading of the derivative work right (§101, §106(2)) treats
 *   any incorporation of copyrighted expression into a new work as preparing
 *   a derivative work requiring authorization. This reading originated in the
 *   1976 Act's broad statutory language ('any other form in which a work may
 *   be recast, transformed, or adapted') but was narrow in practice — courts
 *   required substantial similarity and market substitution. Over 50 years,
 *   through judicial decisions (Salinger v. Random House, Castle Rock v.
 *   Carol Publishing), legislative amendments (DMCA §1201, CASE Act), and
 *   automated enforcement (Content ID), the reading expanded to cover
 *   sampling, quotation, format-shifting, AI training data, and
 *   transformative works. The constraint now operates as a high-extraction
 *   snare: incumbent rightsholders and collecting societies extract licensing
 *   revenue and control from downstream creators who face pre-creation
 *   clearance requirements backed by statutory damages and automated
 *   takedown. The coordination cover story (protecting authors' markets)
 *   persists while the actual function is rent extraction from cultural
 *   production.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.85).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.9).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Comprehensive Derivative Work Enclosure Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '84397fd4-4f39-432f-9311-3893572e9f8f').
narrative_ontology:cs_kernel_codification('84397fd4-4f39-432f-9311-3893572e9f8f', formalized).
narrative_ontology:cs_authority_grounding('84397fd4-4f39-432f-9311-3893572e9f8f', extraction).
narrative_ontology:cs_interpretation_layer_present('84397fd4-4f39-432f-9311-3893572e9f8f').
narrative_ontology:cs_reading_relation('84397fd4-4f39-432f-9311-3893572e9f8f', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('84397fd4-4f39-432f-9311-3893572e9f8f', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('84397fd4-4f39-432f-9311-3893572e9f8f', foundational, derivative_right_is_comprehensive).
narrative_ontology:cs_axiom_status(derivative_right_is_comprehensive, holdable).
narrative_ontology:cs_axiom_grounding('84397fd4-4f39-432f-9311-3893572e9f8f', derivative_right_is_comprehensive, deontological).
narrative_ontology:cs_axiom('84397fd4-4f39-432f-9311-3893572e9f8f', foundational, pre_creation_clearance_required).
narrative_ontology:cs_axiom_status(pre_creation_clearance_required, holdable).
narrative_ontology:cs_axiom_grounding('84397fd4-4f39-432f-9311-3893572e9f8f', pre_creation_clearance_required, conventional).
narrative_ontology:cs_reference_frame('84397fd4-4f39-432f-9311-3893572e9f8f', statutory_derivative_monopoly).
narrative_ontology:cs_drift_state('84397fd4-4f39-432f-9311-3893572e9f8f', post_ai_training_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('84397fd4-4f39-432f-9311-3893572e9f8f', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_rightsholders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, collecting_societies).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, major_publishers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, downstream_innovators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, educational_users).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, archivists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, tech_platforms).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, strong_copyright_protection_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, incentive_theory_of_copyright).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major studios, record labels, and publishing conglomerates that hold large copyright portfolios. They lobby for expansive derivative work definitions, fund enforcement infrastructure (Content ID, DMCA automation), and collect licensing revenue from downstream uses. They set the enforcement agenda through trade associations (MPAA, RIAA, AAP) and direct legislative access.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_rightsholders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, incumbent_rightsholders, beneficiary).

% ASCAP, BMI, SESAC, Harry Fox Agency, and international counterparts. They administer mechanical and performance licenses, collect royalties on derivative uses, and take administrative cuts. Their business model depends on the derivative work right being broad and enforceable pre-creation. They advocate for expanded licensing requirements in every policy forum.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, collecting_societies, beneficiary,
    organized, biographical, constrained, national).

% Large book, music, and journal publishers (Elsevier, Penguin Random House, Universal Music Publishing, etc.). They control access to catalogs, demand clearance for any quotation or adaptation, and use derivative work claims to control downstream markets (translations, adaptations, database rights). They sit on both sides: as rightsholders enforcing against others, and as users seeking licenses from peers.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, major_publishers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, major_publishers, agenda_setter).

% Individual authors, musicians, visual artists, filmmakers who must clear rights for any reference, sample, quotation, or adaptation. They face cease-and-desist demands, Content ID claims, and licensing fees that exceed project budgets. Many self-censor rather than risk infringement. Their exit option is to create only wholly original work — increasingly impossible in a culture saturated with copyrighted expression.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_creators, payer,
    moderate, biographical, constrained, national).

% Remix artists, DJs, modders, fan fiction writers, AI model trainers, documentary filmmakers, parody creators, and software developers building on existing code. Their work inherently incorporates prior expression. Under the enclosure reading, every such use requires prior authorization — creating a thicket of clearance costs that blocks most innovation. They operate in legal gray zones or abandon projects.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, downstream_innovators, payer,
    moderate, biographical, constrained, global).

% Teachers, students, researchers, and educational institutions that need to adapt, translate, annotate, or excerpt works for pedagogy. Fair use provides uncertain defense; institutional risk aversion forces licensing even for clearly transformative educational uses. They are structurally excluded from policy negotiations where derivative work scope is defined.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, educational_users, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, educational_users, excluded).

% Libraries, museums, and digital preservationists who must migrate formats, create access copies, and describe copyrighted works. Format-shifting and metadata creation are treated as derivative preparations requiring permission. Orphan works — where rightsholders cannot be found — remain locked. They have no seat at the table when enforcement regimes expand.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, archivists, payer,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, archivists, excluded).

% Everyday users who meme, quote, remix, and share culture. Platform terms of service and automated enforcement (Content ID, copyright strikes) treat their ordinary cultural participation as infringing derivative preparation. They bear the cost of a permission culture: reduced access, chilled expression, and privatized cultural heritage. No organized representation in copyright policymaking.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, general_public, payer,
    powerless, biographical, trapped, global).

% YouTube, TikTok, Spotify, GitHub, and other platforms that host user-generated content. They build and operate enforcement infrastructure (Content ID, copyright match tools) under pressure from rightsholders and safe harbor requirements. They pay billions in licensing and settlement costs. They also shape the constraint's practical operation: their automated systems define what counts as a derivative work in practice, often more expansively than statute.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, tech_platforms, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, tech_platforms, payer).

% Federal courts interpreting §101 and §106, the Copyright Office administering rulemakings, and Congress amending the Act. They provide the authoritative interpretation layer that absorbs drift (e.g., transformative use doctrine, fair use) without statutory revision. Their decisions legitimate the enclosure reading's expansion while occasionally carving narrow exceptions.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, courts_legislature, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, courts_legislature, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates attribution and compensation for expressive works by granting exclusive control over the preparation of derivative works, theoretically ensuring authors can authorize adaptations and receive royalties.
% TRANSFER_FUNCTION: Moves licensing revenue, clearance authority, and veto power over downstream creation from independent creators, innovators, educators, archivists, and the public to incumbent rightsholders, collecting societies, and major publishers — enforced pre-creation through automated systems and litigation threat.
% ABSENT_VOICES: Future creators whose works do not yet exist but will be constrained by today's derivative work boundaries; users in jurisdictions with broader exceptions (e.g., Canada's fair dealing, EU's quotation right); communities practicing cultural traditions that inherently remix (oral traditions, indigenous knowledge, folk music); orphan work rightsholders who cannot be located to grant permission.
% DISAPPEARANCE_RATIONALE: If the enclosure reading vanished overnight, the entire pre-creation licensing apparatus for derivative works would collapse. Remix, adaptation, translation, annotation, AI training, format-shifting, and cultural commentary would explode without clearance requirements. New creative economies would emerge around transformative use. Cultural heritage institutions could digitize and provide access freely. The power balance would shift from portfolio holders to active creators and users.
% FOUNDING_PROBLEM: Prevent unauthorized commercial exploitation that substitutes for original works in the marketplace and deprives authors of their legitimate market — specifically, verbatim copying and close adaptations that serve as market replacements.
% FOUNDING_PROBLEM_CORROBORATION: The 1976 Act legislative history (House Report 94-1476) shows Congress's concern was market substitution: 'A derivative work must be substantially similar to the original to be an infringement.' Contemporary economists (Boldrin & Levine, Towse), creators' groups (Authors Alliance, Organization for Transformative Works), and the U.S. Copyright Office's own Section 512 and orphan works studies attest the boundary has expanded far beyond substitution to cover any incorporation of protected expression regardless of market effect.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the constraint extracts value from virtually all downstream cultural production — every remix, adaptation, translation, annotation, and AI training run requires permission or faces liability. Suppression is near-maximal (0.9) because alternatives (fair use, de minimis, transformative use) are uncertain, expensive to litigate, and overridden by automated systems that do not weigh exceptions. Theater ratio is moderate (0.3): the 'author protection' justification is real but increasingly disconnected from actual beneficiaries (individual authors receive tiny fractions of licensing revenue; intermediaries capture most). Accessibility collapse is extreme (0.9): once a creator understands the enclosure reading, almost no path exists to create referential work without permission. Resistance is significant (0.7) from fair use litigation, open licensing movements, and platform pushback, but remains fragmented.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent rightsholder seat, the constraint appears as legitimate coordination: they invested in works, built licensing systems, and deserve control over adaptations. From the independent creator seat, the same structure operates as a snare: they cannot afford clearance, fair use is a defense not a right, and automated systems presume infringement. The engine computes this divergence from the structural data — the enclosure reading's claim of 'protecting authors' masks extraction from the very creators it purports to serve.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent rightsholders, collecting societies, and major publishers are structural beneficiaries (d near 0.0): they collect the licensing revenue, control the clearance apparatus, and set enforcement policy. Independent creators, downstream innovators, educators, archivists, and the public are structural targets (d near 1.0): they pay clearance fees, bear litigation risk, self-censor, and lose access — with trapped or constrained exit. Tech platforms sit in a contradictory position: they build and operate the enforcement infrastructure (agenda_setter) but also pay massive licensing costs and face liability (payer) — their derived d is ambiguous. Courts and legislature are analytical observers with agenda-setting power through interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing market-substituting copies) is substantially solved by other means (digital distribution, direct-to-audience models, attribution norms), yet the derivative work right has expanded to cover non-substituting, transformative, and non-commercial uses. The mandate has atrophied into a rent-extraction mechanism. The constraint persists because the beneficiaries (portfolio holders) have institutional power to block reform, and the victims are dispersed, under-resourced, and structurally excluded from policymaking. This is not a piton (no one maintains it theatrically) — it is an actively enforced, expanding snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_any_use_vs_practice,
    'Does ''any use of copyrighted expression'' in the enclosure reading literally mean any use (including de minimis, transformative, non-commercial), or does practice impose unstated limits?',
    'Empirical survey of judicial decisions, Copyright Office guidance, and platform enforcement patterns to identify the actual boundary enforced vs. the statutory text''s literal scope.',
    'If practice imposes limits (e.g., transformative use defense, de minimis), the constraint''s effective extraction is lower than the statutory maximum suggests. If the reading is enforced literally, ε approaches 1.0 for all downstream creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literal_any_use_vs_practice, empirical, 'Gap between statutory text''s literal scope and operational enforcement boundary.').

omega_variable(
    extraction_flow_to_creators_vs_intermediaries,
    'Does the licensing revenue extracted by the derivative work right actually flow to individual authors, or is it captured by intermediaries (publishers, labels, collecting societies)?',
    'Financial tracing of royalty distributions: compare aggregate licensing revenue collected for derivative uses vs. amounts reaching individual creators after intermediary cuts, advances, and administrative fees.',
    'If intermediaries capture most revenue, the constraint''s coordination justification (protecting authors) is falsified — it functions as intermediary rent extraction. This would strengthen the snare classification and weaken any residual rope claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_flow_to_creators_vs_intermediaries, empirical, 'Whether the constraint''s beneficiaries are the authors it claims to protect or the intermediaries that control distribution.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of downstream creation primarily structural (statutory damages, injunctions, automated takedown) or internalized (creators self-censor because they believe any borrowing is wrong)?',
    'Post-exit suppression trajectory: study creators who move to open licensing (CC BY) or public domain — if suppression persists (they still self-censor), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them after legal exit. This would increase measured extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in copyright culture.').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the enclosure reading''s premise foreclose the coordination reading — is there any overlap where both could operate in a single framework?',
    'Formal analysis of the logical relationship between ''any use constitutes derivative work'' and ''only substantial recasting constitutes derivative work'' — identify whether a single legal framework could simultaneously validate both premises for different use classes.',
    'If the readings genuinely foreclose each other (no framework can hold both), the kernel is fundamentally fractured. If they can coexist (different use classes), the kernel supports pluralism and the enclosure reading is one policy choice among others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between sibling readings of the derivative work kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwsb_enclosure_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dwsb_enclosure_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(dwsb_enclosure_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(dwsb_enclosure_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(dwsb_enclosure_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(dwsb_enclosure_tr_t50, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(dwsb_enclosure_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dwsb_enclosure_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(dwsb_enclosure_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(dwsb_enclosure_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(dwsb_enclosure_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(dwsb_enclosure_be_t50, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dwsb_enclosure_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dwsb_enclosure_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(dwsb_enclosure_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(dwsb_enclosure_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(dwsb_enclosure_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(dwsb_enclosure_su_t50, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the statutory derivative work boundary into three structurally distinct readings. The enclosure reading (this story) claims comprehensive coverage with high extraction. The coordination reading claims narrow coverage with minimal extraction (rope/mountain). The hybrid reading claims context-dependent coverage with moderate extraction (tangled_rope). They share the same statutory text (§101, §106(2)) but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__enclosure_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
