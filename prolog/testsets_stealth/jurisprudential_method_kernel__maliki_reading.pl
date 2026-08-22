% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Maliki Source-Doctrine: Medinan Communal Practice ('Amal Ahl al-Madina) as Binding Legal Source
 *   domain: legal/religious/institutional-history
 *
 * SUMMARY:
 *   Within Islamic legal theory, the question of what makes a rule divine law
 *   admits multiple methodological answers; this story authors ONE of them —
 *   the Maliki reading, in which revealed text is read through the continuous
 *   practice of the Medinan community ('amal ahl al-Madina), treated as a
 *   binding source because Medina alone preserved the Prophet's practice
 *   unbroken. The arrangement under assessment is that source-doctrine as it
 *   operated from its crystallization under Malik ibn Anas through the
 *   school's westward institutionalization: it solved a real interpretive
 *   problem (texts underdetermine; recollections conflict) while conferring a
 *   durable advantage on the Medinan lineage that administered the standard
 *   and, later, on the North African judiciary that inherited it, at a
 *   standing cost to jurists elsewhere whose claims to equal authenticity
 *   were priced below Medinan ones. Sibling readings (Hanafi, Shafi'i,
 *   Hanbali) are separate stories linked through the network section; their
 *   epsilon values differ and are not averaged here.
 *
 * KEY AGENTS:
 *   - medinan_scholarly_lineage: agenda-setter and principal beneficiary (institutional/identity_locked) — administers the practice-standard, defines what counts as 'amal, collects the deference and appointments attached to the Medinan chain
 *   - medinan_practicing_community: beneficiary (moderate/constrained) — its inherited ways carry presumptive legal force
 *   - non_medinan_jurists: primary target (organized/constrained) — bears a standing handicap against its rulings in the empire-wide prestige contest
 *   - north_african_qadis: downstream beneficiary (institutional/constrained) — inherits a ready-made authoritative law
 *   - hadith_specialists: target and excluded voice (organized/mobile) — authenticated reports discounted against communal practice; built the textualist alternative
 *   - ordinary_muslim_litigants: dual-positioned (powerless/trapped) — receive stable law; local customs yield to the imported standard
 *   - comparative_jurisprudence_scholars: analytical observer — sees the full four-school structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.52).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.35).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Source-Doctrine: Medinan Communal Practice ('Amal Ahl al-Madina) as Binding Legal Source").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "legal/religious/institutional-history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, 'b54f4c8e-c574-4340-a6a8-21b2d1674e5c').
narrative_ontology:cs_kernel_codification('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', fixed_text).
narrative_ontology:cs_authority_grounding('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', lineage).
narrative_ontology:cs_interpretation_layer_present('b54f4c8e-c574-4340-a6a8-21b2d1674e5c').
narrative_ontology:cs_reading_relation('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', foundational, medinan_practice_binding_source).
narrative_ontology:cs_axiom_status(medinan_practice_binding_source, holdable).
narrative_ontology:cs_axiom_grounding('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', medinan_practice_binding_source, empirically_contingent).
narrative_ontology:cs_axiom('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', secondary, communal_continuity_outweighs_isolated_report).
narrative_ontology:cs_axiom_status(communal_continuity_outweighs_isolated_report, holdable).
narrative_ontology:cs_axiom_grounding('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', communal_continuity_outweighs_isolated_report, empirically_contingent).
narrative_ontology:cs_reference_frame('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', medinan_continuous_practice_standard).
narrative_ontology:cs_drift_state('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', post_shafii_textualist_reform, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b54f4c8e-c574-4340-a6a8-21b2d1674e5c', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_practicing_community).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, north_african_qadis).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, ordinary_muslim_litigants).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, hadith_specialists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, ordinary_muslim_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches, transmits, and adjudicates the method that treats the continuous practice of the Medinan community as a binding source alongside revealed text. Decides what counts as established practice versus mere custom, trains the judges who carry the method abroad, and collects the deference, endowments, and judicial appointments that attach to holders of the Medinan chain. Renouncing the method would mean surrendering the lineage's claim to proximity to the Prophet's community — the entire basis of its standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).

% Lives under a method that declares its inherited ways legally authoritative: local practice carries presumptive force, and residents litigate under rules their grandparents practiced. Bears little direct cost; the burden of justification falls on challengers, who must show a practice contradicts revelation rather than merely cite a contrary report.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_practicing_community, beneficiary,
    moderate, generational, constrained, local).

% Jurists of Kufa, Basra, Baghdad, and Damascus whose own cities' practices and reasoning methods carry no comparable presumption. To win acceptance for a ruling contrary to established Medinan practice they must overcome a standing handicap priced into the method itself. Many retain strongholds where their own approaches govern, but the empire-wide prestige contest values their claims below Medinan ones, and abandoning the contest forfeits their audience.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_jurists, payer,
    organized, biographical, constrained, continental).

% Judges in Qayrawan, Cordoba, and Fez who administer a ready-made body of law backed by the Medinan pedigree. The doctrine supplies authoritative answers without requiring them to reconstruct prophetic practice themselves; appointment and legal certainty flow from alignment with the school that carries it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, north_african_qadis, beneficiary,
    institutional, biographical, constrained, continental).

% Transmitters and critics of prophetic reports based outside Medina. When an authenticated report conflicts with established Medinan practice, the method discounts the report as abrogated, misunderstood, or superseded — subordinating a lifetime of collection and verification to a communal record they cannot audit. They were absent from the circles where the doctrine crystallized, and their successors built the textualist alternative that displaced it across much of the east.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hadith_specialists, payer,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, hadith_specialists, excluded).

% Receive predictable, community-rooted rulings without needing juristic training; marriage, inheritance, and commercial law stay stable across generations. Where their local customs diverge from Medinan norms — Andalusian villages, Saharan towns — their practices yield to the imported standard, and they have no exit from the courts that apply it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, ordinary_muslim_litigants, beneficiary,
    powerless, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, ordinary_muslim_litigants, payer).

% Modern historians and legal theorists who compare the method schools, trace the doctrine's origins and westward spread, and assess the preservation-fidelity claim against transmission evidence. Hold no stake in any school's standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_jurisprudence_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Breaks interpretive deadlock: when revealed texts underdetermine a case or transmitted reports conflict, the continuous practice of the community nearest the Prophet's generation serves as a tie-breaker, giving judges across generations and regions a shared, stable reference standard.
% TRANSFER_FUNCTION: Moves interpretive authority and case outcomes toward Medinan practice norms: deference, scholarly standing, and judicial appointment flow to holders of the Medinan chain and their western inheritors, while rival jurists' rulings carry a standing presumption against them outside their home regions.
% ABSENT_VOICES: Hadith specialists outside Medina, whose authenticated reports the doctrine discounts; Iraqi rationalist jurists, whose methods carried no comparable presumption; and non-Arab provincial communities whose local customs conflicted with Medinan norms — none sat in the Medinan circles where the doctrine crystallized.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, Maliki jurisprudence loses its distinctive anchor: North African and Andalusian courts would need a replacement source-theory, most plausibly a Shafi'i-style textualism; settled rulings resting on 'amal would reopen; and the Medinan lineage's claim to superior proximity — the basis of its standing — would lapse.
% FOUNDING_PROBLEM: After the Prophet's death the community faced novel cases with fragmentary textual guidance and conflicting recollections of his practice; some authoritative tie-breaker between competing claims about divine law was needed.
% FOUNDING_PROBLEM_CORROBORATION: Rival-school engagement corroborates both the problem and its contested status: al-Shafi'i's al-Umm treats Medinan practice as evidence serious enough to require systematic rebuttal while denying its binding status, and Hanafi and Hanbali literature concede the interpretive-underdetermination problem while proposing different anchors. Modern academic historiography corroborates that a distinct, continuous Medinan legal tradition existed, while disputing the strength of the fidelity claim. No source outside the benefiting parties attests that the fidelity claim itself is settled.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is medium (0.52): the doctrine performs genuine anchoring work — a continent of judges received stable answers — while embedding an asymmetric privilege whose rent accrues to the administering lineage. Suppression (0.35) is real but bounded: enforcement concentrated in contested eastern centers and normalized into education and courts in the western heartland, which the suppression series models as a rise-then-easing hump rather than a monotonic ratchet. Theater (0.25, rising) tracks provincial invocation of 'the practice of Medina' increasingly substituting for actual engagement with Medinan sources as the school's center of gravity moved thousands of kilometers from Medina. Accessibility collapse is low (0.35): rival schools persisted and flourished, so alternatives never collapsed. Resistance is substantial (0.55): the Shafi'i reform was a direct methodological assault on the 'amal doctrine, and Hanafi counter-prestige contested it continuously. All three tracked metrics run on one shared six-point grid. Claim and metrics are independent authored facts: tangled_rope is claimed from structure (both a coordination function and enforced asymmetric extraction are present), not tuned to any predicted output.
 *
 * PERSPECTIVAL GAP:
 *   From the Medinan lineage's seat the doctrine is fidelity: continuity with the Prophet's own community, the most conservative possible anchor. From the non-Medinan jurist's seat the same structure is a rigged prestige market in which their claims are priced down by a presumption they never consented to. From the hadith specialist's seat it is evidence-subversion: verified transmission chains outranked by an unauditable communal memory. From the litigant's seat it is stability in the heartland and arbitrariness at the margins. The engine computes this per-seat divergence from the power and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (medinan_scholarly_lineage, medinan_practicing_community, north_african_qadis) derive low directionality — the constraint subsidizes them, damping effective extraction toward or below zero for those seats. The declared victim group (non_medinan_jurists) derives high directionality, amplified by constrained exit: they cannot leave the legitimacy contest without forfeiting their audience. Hadith specialists, declared as payers, sit near the target end but their mobile exit (they built the successful alternative) tempers amplification. Ordinary litigants carry dual declaration and land near symmetric. The school's continental spatial scope raises verification difficulty, scaling effective extraction modestly upward for target seats — a doctrine spanning a continent is harder to audit than one confined to Medina.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — authoritative tie-breaking for textual underdetermination — remains partially live: revealed texts still underdetermine cases. But the specific solution (privileging one city's unrecorded practice) has been epistemically transformed by matured hadith science, which can now audit transmission in ways eighth-century Medina could not. The classification guards against two opposite errors: reading the doctrine as pure extraction ignores the real anchoring function that made it persuasive across a continent for centuries; reading it as pure coordination ignores the priced-down rival claims its enforcement maintained. The founding_problem_status=contested x disappearance_verdict=world_rearranges combination is the load-bearing signal: the arrangement persists because institutions depend on it, not because the founding problem is settled — exactly the profile in which mandate-outlived-function drift must be watched rather than assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the maliki_reading of the jurisprudential_method_kernel; how would the structural assessment change under the sibling readings?',
    'Generate the hanafi, shafii, and hanbali reading stories and compare per-seat classifications and epsilon across the family.',
    'Under the shafii reading the beneficiary/victim structure largely inverts — transmission specialists gain standing and practice-based claims pay; under the hanbali reading both communal-practice and reason-based sources lose to literal text. Cross-family comparison is the only way to detect whether any reading''s privilege tracks truth-tracking rather than position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one reading of a four-reading kernel; sibling readings are separate constraints.').

omega_variable(
    medinan_preservation_fidelity,
    'Did Medinan communal practice actually preserve prophetic practice more faithfully than isnad-transmitted reports — the empirical premise on which the doctrine''s legitimacy rests?',
    'Comparative hadith criticism: matn analysis, Medinan-versus-Iraqi report corpora, and historiography of early Medinan legal practice.',
    'If the fidelity claim holds, a large share of the measured extraction is the price of genuine coordination; if it fails, the doctrine functions as parochial rent wearing a piety costume and the classification drifts toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_preservation_fidelity, empirical, 'Whether the doctrine''s foundational historical premise is true.').

omega_variable(
    doctrine_vs_lineage_interest,
    'Does the doctrine track legal truth-seeking or the Medinan lineage''s institutional position — would the lineage have defended ''amal in cases where it cut against Medinan interests?',
    'Case-level analysis of Maliki rulings adverse to Medinan practice or interest, and of the lineage''s behavior when rival methods produced socially superior outcomes.',
    'Consistent defense of ''amal even against lineage interest supports the coordination reading; selective defense tracking interest supports a capture reading and raises effective extraction for the administering seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_lineage_interest, conceptual, 'Whether the doctrine''s persistence reflects method or institutional self-interest.').

omega_variable(
    post_textualist_persistence,
    'After hadith science matured, does continued reliance on ''amal in Maliki territories reflect ongoing epistemic function or institutional inertia maintained by school identity?',
    'Examine contemporary Maliki usul pedagogy and fatwa practice: critical engagement with the fidelity premise versus ritual invocation of the doctrine''s label.',
    'Sustained theatrical reliance would signal drift toward degraded, inertia-maintained operation; renewed critical defense of the fidelity claim would indicate the function remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_textualist_persistence, empirical, 'Whether the doctrine''s persistence is functional or inertial in the post-textualist era.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(juri_tr_t50, jurisprudential_method_kernel__maliki_reading, theater_ratio, 50, 0.11).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__maliki_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(juri_tr_t150, jurisprudential_method_kernel__maliki_reading, theater_ratio, 150, 0.19).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement(juri_tr_t250, jurisprudential_method_kernel__maliki_reading, theater_ratio, 250, 0.25).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(juri_be_t50, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 100, 0.45).
narrative_ontology:measurement(juri_be_t150, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(juri_be_t250, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 250, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(juri_su_t50, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 50, 0.34).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(juri_su_t150, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 150, 0.43).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 200, 0.39).
narrative_ontology:measurement(juri_su_t250, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 250, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Islamic legal methodology' covers four structurally distinct source-hierarchy arrangements; per the epsilon-invariance principle they are authored as separate stories sharing one kernel. The maliki reading sits mid-family: it presupposes the kernel's scriptural commitment, and its 'amal doctrine is the specific claim the shafii reading was constructed to refute, so edges run to all three siblings. Each member carries its own epsilon, beneficiary/victim structure, and classification; nothing is averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
