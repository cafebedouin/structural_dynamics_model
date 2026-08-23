% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hybrid Continuity Account of the Hebrew Vernacular Revival
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This story instantiates the hybrid continuity account of how Hebrew
 *   passed from some seventeen centuries of liturgical-literary maintenance
 *   without a native speech community into a spoken national vernacular. On
 *   this reading the transformation had two jointly necessary components: a
 *   preserved substrate (biblical, rabbinic, and medieval liturgical and
 *   literary usage, sustained by continuous recitation, schooling, and
 *   textual commentary) and a deliberate reconstruction (mass teaching
 *   networks, terminology coinage, phonological settling, grammatical
 *   regularization, and state consolidation) that converted preserved
 *   material into daily vernacular life. Neither component sufficed alone:
 *   preservation supplied forms and symbolic anchoring but no speakers;
 *   reconstruction supplied speakers and domains but drew its raw material
 *   and its legitimacy from the preserved layer. The epsilon referent is the
 *   standing historical arrangement itself, the actual
 *   preservation-then-revival sequence, assessed by this reading's own
 *   lights: a functional division of labor across generations in which no
 *   group bears systematic net cost through the account's operation, hence
 *   low extractiveness. The claimed type and the authored metrics are
 *   independent facts: rope is what this reading believes is structurally
 *   true of itself, and the metrics below describe its observed operation
 *   without being tuned to any predicted engine output. KEY AGENTS (by
 *   structural relationship): - hebrew_linguistics_researchers: administering
 *   beneficiary seat (organized/mobile) — maintains and teaches the
 *   two-factor account, collects its citation authority -
 *   hebrew_language_academy: institutional beneficiary
 *   (institutional/constrained) — reconstruction mandate validated by the
 *   account - hebrew_teaching_institutions: practitioner beneficiaries
 *   (organized/constrained) — ulpan and school networks where the two
 *   components meet - international_revival_movements: downstream
 *   beneficiaries (organized/mobile) — import the
 *   substrate-plus-reconstruction template - modern_hebrew_speakers: outcome
 *   population, identity-locked beneficiaries (organized/identity_locked) -
 *   liturgical_preservation_communities: excluded seat
 *   (organized/identity_locked) — performed the substrate-preserving labor,
 *   absent from the debate that classified it -
 *   comparative_linguistics_observers: analytical observer
 *   (analytical/analytical) — sees the full cross-case structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.16).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.22).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hybrid Continuity Account of the Hebrew Vernacular Revival").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '70e33c0e-06d9-4ba8-9b82-76834ac88475').
narrative_ontology:cs_kernel_codification('70e33c0e-06d9-4ba8-9b82-76834ac88475', distributed).
narrative_ontology:cs_authority_grounding('70e33c0e-06d9-4ba8-9b82-76834ac88475', expertise).
narrative_ontology:cs_interpretation_layer_present('70e33c0e-06d9-4ba8-9b82-76834ac88475').
narrative_ontology:cs_reading_relation('70e33c0e-06d9-4ba8-9b82-76834ac88475', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('70e33c0e-06d9-4ba8-9b82-76834ac88475', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('70e33c0e-06d9-4ba8-9b82-76834ac88475', foundational, substrate_reconstruction_joint_necessity).
narrative_ontology:cs_axiom_status(substrate_reconstruction_joint_necessity, holdable).
narrative_ontology:cs_axiom_grounding('70e33c0e-06d9-4ba8-9b82-76834ac88475', substrate_reconstruction_joint_necessity, empirically_contingent).
narrative_ontology:cs_axiom('70e33c0e-06d9-4ba8-9b82-76834ac88475', foundational, revival_credit_shared_across_contributing_generations).
narrative_ontology:cs_axiom_status(revival_credit_shared_across_contributing_generations, holdable).
narrative_ontology:cs_axiom_grounding('70e33c0e-06d9-4ba8-9b82-76834ac88475', revival_credit_shared_across_contributing_generations, deontological).
narrative_ontology:cs_reference_frame('70e33c0e-06d9-4ba8-9b82-76834ac88475', substrate_reconstruction_joint_necessity).
narrative_ontology:cs_drift_state('70e33c0e-06d9-4ba8-9b82-76834ac88475', post_revisionist_revivalistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('70e33c0e-06d9-4ba8-9b82-76834ac88475', '2026-06-14T09:30:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_linguistics_researchers).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_teaching_institutions).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, international_revival_movements).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speakers).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, unbroken_textual_transmission_record).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, planned_lexical_elaboration_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Studies and teaches the account of Hebrew's passage from liturgical maintenance to vernacular life, publishing the two-factor causal model in monographs, handbooks, and curricula. The account's authority flows to the community as citation standing and program-building capital, which members spend competitively rather than bank. Senior members set syllabi and handbook chapters, giving the community an administrative hand in where the account circulates. Individual exit is cheap: a scholar can dissent, reframe, or found a rival program without losing professional standing.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_linguistics_researchers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, hebrew_linguistics_researchers, agenda_setter).

% Chartered body that coins terminology, standardizes grammar, and arbitrates usage for Hebrew speakers worldwide. The account validates its reconstructive labor as historically necessary rather than artificial meddling, feeding its institutional mandate and public legitimacy. Its exit is bound by charter: it cannot abandon the language-planning role without ceasing to be itself.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_language_academy, beneficiary,
    institutional, generational, constrained, global).

% Ulpan networks, Israeli school systems, and diaspora Hebrew programs that operationalize reconstruction pedagogically, teaching the language as a living vernacular built on classical layers. The account legitimizes their method; their classrooms are where the transmitted layer and the modernizing layer meet in daily practice. Redirecting pedagogy toward rival framings carries moderate curricular cost but is feasible.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_teaching_institutions, beneficiary,
    organized, generational, constrained, global).

% Welsh, Maori, Gaelic, and similar language revitalization efforts that import the substrate-plus-reconstruction template as strategy: protect the transmitted layer while investing in elaboration, coinage, and intergenerational transmission. They take the template freely, adapt it to local conditions, and incur no obligation to its originators.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, international_revival_movements, beneficiary,
    organized, generational, mobile, global).

% People who acquire and live in Hebrew as a native vernacular, in Israel and diaspora communities. They inherit both the language and its public self-account: that their speech descends jointly from preserved liturgical layers and from deliberate reconstruction. Their lives run through the language, so leaving it is not a practical option; they neither administer the account nor pay into it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speakers, beneficiary,
    organized, generational, identity_locked, global).

% The transgenerational chain of communities whose daily recitation, manuscript copying, schooling, and commentary maintained Hebrew's liturgical-literary layer across the centuries without a native speech community. The scholarly debate retrospectively classified that practice as the substrate component; they were never seated in the journals that assigned the label. Their plausible objection, that devotion is being inventoried as raw material, currently has no venue. Their practice continues regardless of how it is classified.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_preservation_communities, excluded,
    organized, civilizational, identity_locked, global).

% Scholars comparing revival outcomes across cases (Celtic, Polynesian, engineered languages) who read the Hebrew account from outside its beneficiary circle, weighing how much of its structure generalizes. They hold no stake in the credit allocation among contributing generations and can adopt or discard the template analytically.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, comparative_linguistics_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__hybrid_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_vitality__hybrid_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes the causal account of Hebrew's vernacularization into a shared two-component model (preserved substrate plus deliberate reconstruction), letting religious-continuity and pioneer-reconstruction narratives occupy one explanatory framework instead of blocking each other, and exporting a reusable template to other language-revival efforts.
% TRANSFER_FUNCTION: Moves interpretive credit and disciplinary authority: allocates recognition of Hebrew's survival between liturgical custodial generations and modernizing reconstructors, transferring citation authority and curricular legitimacy toward holders of the two-factor account, and secondarily transfers the strategic template outward to revival movements elsewhere.
% ABSENT_VOICES: Liturgical custodial generations, whose recitation, copying, and schooling preserved the substrate, were never seated in the sociolinguistic debate that retrospectively classified their practice; their plausible objection has no journal. Pre-state ulpan cohorts and the coinage-era terminologists likewise shaped the outcome without seats in the retrospective framing. They sit outside the academic conversation, in ritual communities and family memory.
% DISAPPEARANCE_RATIONALE: Textbooks and curricula would lose their stabilizing causal account and the field would revert to a binary contest between continuity-maximalist and invention-maximalist narratives; revival-movement strategists would lose the imported two-component template; proponents of each pole would regard the loss as trivial, arguing their own accounts were the substantive contenders all along. Whether the world rearranges depends on which seat answers.
% FOUNDING_PROBLEM: After vernacularization succeeded, the community faced a legitimacy split over Hebrew's origin: a continuity narrative ('it never died') and an invention narrative ('it was engineered') each delegitimized part of the communal story and each conflicted with part of the linguistic evidence. Scholarship needed an account crediting both preservation and reconstruction enough to reconcile communal identity with the evidence.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Zionist language politics attest the continuity-versus-invention dispute from outside the sociolinguistic beneficiary set (period press controversies, curriculum conflicts documented in educational histories); diaspora Hebrew-teaching organizations independently attest the strategic need for a creditable two-part account. No corroboration exists from the liturgical custodial communities themselves; their absence from the attestation record is itself signal.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16) because the account accrues interpretive authority without extracting rents: it rhetorically demotes pole-exclusive origin claims, which is a discourse-internal cost, not a structural transfer. Suppression is low-moderate (0.22, authored raw and unscaled per the structural-property rule) because the account dissolves a binary by persuasion rather than force; rival framings remain publishable and live. Theater is minimal (0.08): a plain analytical claim with slight boilerplate ritualization in textbook summaries. Accessibility_collapse is moderate (0.48) because accepting the frame settles the causal question while leaving definitional stances about what vitality IS partly open; alternatives do not vanish. Resistance is moderate-high (0.55) because traditionalist readers resist the insufficiency clause and purist readers resist the continuity clause. Measurements run on one shared time grid (t=0,6,12,18,24,30, mapping roughly to 1948-2025) with both tracked metrics authored at every point; the gentle rise in base_extractiveness tracks the account's consolidation from corrective to canonical, mildly concentrating citation authority. A suppression_requirement series is deliberately NOT authored: the enforcement picture is static (an argumentative constraint with no enforcement machinery whose capacity neither builds nor decays), so the scalar in base_properties carries that fact. Receipt check performed per seat before authoring gain_flow: researchers accrue citation authority but spend it competitively rather than banking it as rent; the Academy accrues mandate-legitimacy without controlling any rate; no seat converts the account's authority into captured extraction, so 'diffuse' is authored as an affirmative checked claim, not a default. Fixing cost is 'cheap': revising or replacing the account proceeds through ordinary publication, with no sunk infrastructure binding it.
 *
 * PERSPECTIVAL GAP:
 *   Seats at the same nominal epistemic level (literate participants in one discourse) should compute differently. The researcher seat experiences the account as coordination it built and administers; the Academy seat experiences it as validation of a chartered mandate; teaching institutions experience it as professional legitimacy for their method; revival movements experience it as an importable strategy. The excluded liturgical seat, were it seated, would likely compute the same classification of its practice as appropriation of unrecognized labor: devotion inventoried as raw material. The engine computes this divergence from the structural data (power, exit, declared position); the divergence between the mobile researcher seat and the identity-locked speaker and liturgical-community seats is driven by constraint-specific exit asymmetry, not by global standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Five beneficiary declarations drive low derived directionality for those seats: the account subsidizes them with recognition, legitimacy, or usable strategy, and none bears a net structural cost. No victims are declared because no group bears net structural cost through the account's operation; the nearest candidates, proponents of exclusive-origin narratives, are parties of the sibling constraints and are deliberately not folded into this file. Suppression is noted as a raw structural property, unscaled by power or scope; scope amplification of effective extraction is the engine's arithmetic, fed here by the global scopes all seats carry. No directionality overrides are used: the derivation from beneficiary declarations and exit options produces accurate d values for every seat, and the one residual nuance (researchers collecting mild citation authority through their administrative secondary role) is too small and too diffuse to warrant an override keyed to a power atom shared with correctly low-d seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live (identity-versus-evidence contests recur) and the disappearance verdict is contested, so no dead-mandate/world-rearranges mismatch arises and no zombie flag is warranted. Classification discipline runs in both directions here: without the two-factor structure, a preservation-only account invites a piton reading of liturgical maintenance as theatrical recitation, while an invention-only account invites a snare reading of planned reconstruction as fabricated imposition. The hybrid account keeps the genuine coordination visible (standardized causal framework, net-benefiting participants) while its lack of enforcement keeps it out of tangled_rope territory. The main risk to the rope reading is internal: if the necessity clause fails empirically, the account collapses toward a reconstruction-primary variant outside the current reading set, flagged in omega preservation_necessity_counterfactual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel hebrew_vitality, namely the hybrid_continuity_reading. What would the classification surface look like under the sibling readings, and where exactly is the disagreement located?',
    'Compile and compare the sibling stories (hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading): the liturgical reading shifts the epsilon referent to unbroken liturgical continuity as the arrangement under contest; the native-daily reading dismisses custodial practice as non-life and raises definitional stakes. Cross-file comparison localizes the disagreement to the definition of vitality and the allocation of causal credit.',
    'Under the liturgical reading, preservation''s status rises from necessary-enabler to constitutive whole and this file''s low epsilon would be re-based onto liturgical continuity; under the native-daily reading, the substrate''s credit drops toward zero and the reconstruction term dominates. This file''s rope classification holds only under the hybrid referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel membership and reading index of this story; sibling deltas and the locus of disagreement.').

omega_variable(
    definitional_sidestep_underdetermination,
    'Does the hybrid reframing actually resolve the vitality contest, or merely relocate it from the definitional axis (what vitality IS) to the causal axis (what produced vernacularity), leaving the siblings'' definitional claims intact underneath?',
    'Track whether scholars who adopt the two-factor causal account converge on a shared definition of vitality or retain pole-aligned definitions beneath shared causal vocabulary; convergence indicates resolution, persistent definitional stratification indicates relocation.',
    'If merely relocated, this reading is a mediation overlay rather than a resolution; its coordination function is thinner than the rope reading assumes and the classification should weight definitional persistence accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_sidestep_underdetermination, conceptual, 'Whether reframing resolves or relocates the underlying contest.').

omega_variable(
    substrate_reconstruction_proportionality,
    'In what proportion does modern Hebrew derive from continuous substrate versus deliberate reconstruction, across phonology, morphology, syntax, and lexicon?',
    'Corpus-based morphosyntactic and phonological provenance studies tracing features to strata (biblical, rabbinic, medieval versus contact-induced and academy-coined material) on comparative diachronic corpora.',
    'A high reconstruction share strengthens the insufficiency clause and this reading against continuity-maximal framings; a low share tilts weight toward the liturgical reading''s continuity emphasis and could soften the necessity distinction this file turns on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_reconstruction_proportionality, empirical, 'Quantitative substrate-versus-reconstruction composition of the revived language.').

omega_variable(
    preservation_necessity_counterfactual,
    'Was liturgical preservation genuinely necessary for vernacular revival, or could a deliberately engineered language lacking the substrate have achieved comparable vernacular uptake?',
    'Comparative analysis across revival and engineering cases differing in substrate depth while holding planner resources and community motivation approximately constant (Esperanto-family contrasts, Celtic and Polynesian revivals); causal identification is imperfect by the nature of the counterfactual.',
    'If necessity fails, the hybrid reading collapses toward a reconstruction-primary variant outside the declared reading set and this file''s axioms require revision; if necessity holds robustly, the joint-necessity axiom is corroborated and this file''s foreclosure edge toward the liturgical reading sharpens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preservation_necessity_counterfactual, empirical, 'Testability of the necessity clause via cross-case counterfactual comparison.').

omega_variable(
    beneficiary_diffuseness_ambiguity,
    'Are the five declared beneficiaries genuine net beneficiaries of the account''s operation, or merely unharmed parties receiving incidental recognition?',
    'Counterfactual welfare comparison per seat: assess each declared seat''s position under a rival dominant account; seats indifferent across accounts are incidental rather than structural beneficiaries.',
    'If all benefits are incidental, the rope reading loses its participant-net-benefit leg and the constraint drifts toward a no-party analytical profile, triggering reclassification review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_diffuseness_ambiguity, conceptual, 'Whether declared beneficiary structure is structural or incidental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_tr_t0, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t6, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 6, 0.05).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_tr_t6, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t12, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 12, 0.06).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_tr_t12, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t18, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 18, 0.07).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_tr_t18, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t24, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 24, 0.07).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_tr_t24, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_tr_t30, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.09).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_be_t0, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t6, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_be_t6, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t12, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 12, 0.13).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_be_t12, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t18, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 18, 0.14).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_be_t18, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t24, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_be_t24, observed).
narrative_ontology:measurement(hebrew_vitality_hybrid_be_t30, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement_basis(hebrew_vitality_hybrid_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__hybrid_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Hebrew vitality' conflates three structurally distinct claims with different epsilon referents and different beneficiary structures: whether unbroken liturgical use itself constitutes vitality (liturgical_reading), whether only native daily generation constitutes it (native_daily_reading), and whether vernacular vitality required joint substrate-and-reconstruction causation (this file). Decomposed per the epsilon-invariance principle; this story's epsilon is indexed to the hybrid causal-synthesis referent and must not be averaged with its siblings'. Family links carried in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
