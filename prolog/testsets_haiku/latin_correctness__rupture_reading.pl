% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Classical Latin Purity Standard (Rupture Reading)
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the 'rupture reading' of the contested
 *   kernel 'latin_correctness': the claim that Classical Latin is a fixed
 *   textual standard accessible only through reconstruction from ancient
 *   sources, and that medieval Latin usage constitutes corruption of that
 *   standard. This reading emerged during the Renaissance humanist movement
 *   (14th–16th centuries) as scholars like Petrarch and Valla elevated
 *   classical texts as sole authorities on correct Latin usage. Under this
 *   reading, medieval Latinists become victims of a retroactively imposed
 *   standard they cannot meet without abandoning their embodied knowledge and
 *   transmitted practices. The high extractiveness (0.81 at interval end)
 *   reflects the constraint's function as a legitimacy-stripping mechanism:
 *   medieval expertise is reclassified as failure; institutional authority
 *   migrates to classicists; practical domains lose the ability to innovate
 *   linguistically without stigma. The rupture reading coexists with (but
 *   does not foreclose) the continuity reading, which treats medieval Latin
 *   as legitimate linguistic evolution, and the hybrid reading, which permits
 *   classical norms in literary domains while accepting medieval forms in
 *   technical domains. This reading's distinctive claim — that the ancient
 *   textual standard is THE legitimate reference, and all divergence is
 *   corruption — is empirically contestable (modern linguistics shows
 *   medieval change as evolution, not corruption) and permits no compromise
 *   that preserves the rupture's core premise.
 *
 * KEY AGENTS:
 *   - Humanist scholars: Institutional beneficiaries who control the reconstruction and certification of classical standards
 *   - Medieval Latinists: Moderate-power victims, identity-locked to embodied medieval practices
 *   - Technical writers: Moderate-power victims whose practical neologisms are delegitimized
 *   - Vernacular practitioners: Indirect victims whose legitimacy is suppressed by the purity claim
 *   - Manuscript copyists: Powerless trapped victims, retroactively judged for adaptive transmission
 *   - Classical philologists: Institutional beneficiaries whose expertise is elevated to sole authority
 *   - Church authority: Institutional agenda-setter and beneficiary, enforcing curriculum alignment
 *   - Classical texts: Non-agent vindicated entity, elevated above the living tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.81).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.76).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, snare).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin Purity Standard (Rupture Reading)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, 'a112f0e2-a1d1-4474-998b-2559d9723490').
narrative_ontology:cs_kernel_codification('a112f0e2-a1d1-4474-998b-2559d9723490', fixed_text).
narrative_ontology:cs_authority_grounding('a112f0e2-a1d1-4474-998b-2559d9723490', extraction).
narrative_ontology:cs_interpretation_layer_present('a112f0e2-a1d1-4474-998b-2559d9723490').
narrative_ontology:cs_reading_relation('a112f0e2-a1d1-4474-998b-2559d9723490', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a112f0e2-a1d1-4474-998b-2559d9723490', latin_correctness__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a112f0e2-a1d1-4474-998b-2559d9723490', foundational, classical_texts_sole_authority).
narrative_ontology:cs_axiom_status(classical_texts_sole_authority, holdable).
narrative_ontology:cs_axiom_grounding('a112f0e2-a1d1-4474-998b-2559d9723490', classical_texts_sole_authority, conventional).
narrative_ontology:cs_axiom('a112f0e2-a1d1-4474-998b-2559d9723490', foundational, medieval_divergence_is_corruption).
narrative_ontology:cs_axiom_status(medieval_divergence_is_corruption, overridden).
narrative_ontology:cs_axiom_grounding('a112f0e2-a1d1-4474-998b-2559d9723490', medieval_divergence_is_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('a112f0e2-a1d1-4474-998b-2559d9723490', classical_textual_purity_doctrine).
narrative_ontology:cs_drift_state('a112f0e2-a1d1-4474-998b-2559d9723490', contemporary_historical_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a112f0e2-a1d1-4474-998b-2559d9723490', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_restoration_movement).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_latinists).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, technical_writers).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, church_authority).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, manuscript_copyists).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, classical_linguistic_purity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, enforce, and transmit the classical standard through education, textual criticism, and institutional gatekeeping. They control which texts count as exemplary, which usages are 'correct,' and which scholars are credible. Their authority rests on reconstructing the classical canon from fragmentary sources and declaring that reconstruction the legitimate reference. They benefit through professional prestige, institutional authority, and the ability to certify or condemn other scholars' work.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, humanist_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).

% Practice a living Latin tradition inherited from the medieval period — one that diverges from the reconstructed classical standard in grammar, vocabulary, and syntax. Under the rupture reading, their entire body of work becomes classified as 'corrupt,' their methods delegitimized, their scholarly authority questioned. Their exit options are severely constrained: abandoning Latin entirely means surrendering centuries of transmitted knowledge; adopting the classical standard requires unlearning embodied linguistic practices and retraining in a reconstructed grammar based on ancient texts they may not have full access to. Their professional identity and career advancement depend on Latin authority being negotiable rather than fixed.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_latinists, payer,
    moderate, biographical, identity_locked, continental).

% Use Latin for practical domains — law, medicine, theology, administration — where medieval innovations and neologisms address contemporary problems the classical vocabulary cannot cover. The rupture standard treats their necessary linguistic extensions as failures of fidelity rather than linguistic solutions. They bear the cost of either adopting an inadequate classical vocabulary or accepting the stigma of non-compliance. They are partially excluded from the scholarly conversation about Latin's correctness because the rupture reading pre-judges their work.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, technical_writers, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, technical_writers, excluded).

% Develop and defend the legitimacy of writing and formal thought in emerging vernacular languages (Italian, French, German). The rupture reading's assertion of absolute classical purity indirectly delegitimizes their work: if perfect Latin is accessible and required for intellectual authority, vernacular writing becomes confession of linguistic failure rather than legitimate linguistic choice. They bear the cost of cultural marginalization, and their exit options are limited because the rupture standard's authority constrains institutional pathways for vernacular-language scholarship.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_practitioners, payer,
    moderate, biographical, constrained, continental).

% Reproduce texts across the medieval period, often adapting them to local usage patterns and comprehensibility. The rupture reading reinterprets their adaptive practices — previously understood as skilled transmission — as corruption. They have no institutional voice to defend their choices and cannot access the classical sources the standard now requires as reference. They are trapped in a condition retroactively declared incorrect by standards not in effect when they worked.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, manuscript_copyists, payer,
    powerless, biographical, trapped, local).

% Specialize in the reconstruction and interpretation of classical texts. The rupture reading elevates their expertise to the highest level of legitimacy: they are the sole custodians of the correct standard and its historical sources. They benefit from the prestige of serving as authorities on linguistic purity and from the institutional resources that support classical-text scholarship (library access, patronage, institutional positions).
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classical_philologists, beneficiary,
    institutional, generational, arbitrage, continental).

% Controls educational institutions (monasteries, cathedral schools, universities) and can enforce curriculum standards. The rupture reading aligns with church authority's interest in standardized theology and doctrine — a fixed classical Latin standard enables centralized doctrinal control. However, the church also depends on medieval Latin for practical administration and theology, creating internal tension (not fully resolved in this reading). Church authority benefits from the standard's enforcement machinery but bears some costs from its practical linguistic constraints.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, church_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, church_authority, beneficiary).

% Witness the displacement of medieval Latin expertise and the elevation of classical authority. They can analyze the beneficiary/victim structure and contest the reading's naturalness claims, but institutional gatekeeping limits their ability to reshape the standard once it is established.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, contemporary_observers, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, measurable reference standard for Latin usage across institutions and time periods, enabling scholars to evaluate textual authenticity and linguistic correctness against a fixed criterion rather than navigating multiple, context-dependent traditions.
% TRANSFER_FUNCTION: Moves intellectual authority and institutional prestige from medieval scholars and practical Latin users to humanist classicists and classical philologists. Medieval practitioners' embodied knowledge and transmitted expertise are reclassified as corruption, and their authority to set standards is transferred to those controlling access to ancient sources and their reconstruction.
% ABSENT_VOICES: The voices of medieval Latinists, practical-domain technical writers, and manuscript copyists are structurally excluded from the conversation that defines the standard — their knowledge is pre-judged as corrupt before they can defend their practice. Vernacular advocates are excluded because the rupture reading exists partly to suppress their legitimacy.
% DISAPPEARANCE_RATIONALE: If the rupture reading and its enforcement machinery disappeared, medieval Latin practices would be rehabilitated as legitimate tradition rather than corruption, institutional positions for medieval scholars would remain viable, technical domains could continue neologistic innovation without stigma, and vernacular development would lose a major institutional delegitimizer. The authority to define Latin correctness would devolve back to the living tradition rather than remaining centralized in classicists.
% FOUNDING_PROBLEM: Late-medieval and Renaissance scholars observed textual corruption and divergence from classical models in manuscripts and current usage; they sought a method to distinguish authentic classical usage from later accretions and alterations, enabling the recovery of original ancient texts and their correct interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars attest that textual corruption and medieval deviation from classical norms are empirically real problems that rigorous reconstruction solves. Medieval defenders and modern linguistic historians attest that the founding problem overstates divergence and mischaracterizes medieval practice as failure rather than legitimate innovation — they point to the continuity of grammatical core and the functional adequacy of medieval forms for their domains. Modern historical linguists outside the humanist camp note that all living languages change; the founding problem's framing assumes classical fixity is natural rather than constructed.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.81 across the interval as institutional adoption of the rupture reading deepens. The constraint begins as a scholarly claim (1350–1400, mid-range extractiveness) but becomes enforced policy through university curricula and church authority's educational control by 1500–1600, driving higher extraction. Suppression rises symmetrically (0.28 to 0.76) as the mechanism shifts from persuasion to institutional enforcement — medieval scholars' work is increasingly rejected in hiring, publication, and theological training. Theater_ratio rises from 0.12 to 0.42, indicating growing performative maintenance: by 1600, much energy goes to demonstrating fidelity to the classical standard (correcting manuscripts, writing commentaries) rather than advancing practical Latin use or developing the language. Resistance declines from 0.62 to 0.38 as institutional capture hardens and medieval alternatives lose institutional patronage. Accessibility_collapse reaches 0.68 at interval end: medieval practitioners cannot access the classical sources required to adopt the standard, and no institutional pathway permits them to remain credible while maintaining medieval practices. The time grid aligns across all metrics and all levels: the coercion intensifies from individual (stylistic preference) through organizational (curriculum requirement) to structural (canon formation, institution gatekeeping) as the reading's authority consolidates.
 *
 * PERSPECTIVAL GAP:
 *   The humanist-scholars/church-authority seats and the medieval-latinists/technical-writers seats compute sharply different constraint types from the same structural data. From the beneficiary seat (humanist scholar or classical philologist), the constraint appears as legitimate coordination — the real problem of textual corruption is solved by establishing an objective standard accessible to all willing to study classical sources. From the victim seats (medieval Latinists, technical writers), the same structure operates as extractive enforcement: a standard is imposed that they cannot meet without abandoning their knowledge, their exit options are closed through institutional gatekeeping, and their expertise is delegitimized without their participation in setting the standard. The engine computes this divergence from power (beneficiaries are institutional, victims are moderate/powerless), exit_options (beneficiaries have arbitrage or mobile options, victims are identity_locked or trapped), and beneficiary/victim declaration. The authored claim (snare) aligns with the victim-seat perspective; the beneficiary seats believe the claim is rope (coordination solving a real problem). The metrics describe high extraction + substantial suppression + meaningful resistance, which supports the snare classification at the victim seats and flags potential false-rope dynamics at the beneficiary seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars hold d ≈ 0.1 (full beneficiary): they set and enforce the standard, control institutional gatekeeping, and collect prestige and authority. Medieval Latinists hold d ≈ 0.85 (near-full target): their embodied practices are delegitimized, their institutional viability depends on adopting a standard they cannot access without retraining, their identity as Latinists is classified as corrupt. Technical writers hold d ≈ 0.8: they need Latin for their domains but cannot innovate without incurring stigma; their practical solutions are reclassified as failures of fidelity. Vernacular practitioners hold d ≈ 0.75 indirectly: the rupture reading's authority suppresses their legitimacy as an alternative. Church authority holds d ≈ 0.2 (near-beneficiary): it enforces the standard and gains standardization benefits, but also bears some costs from the practical linguistic constraints the classical purity imposes. The beneficiary/victim declarations drive these directional positions: those listed as beneficiaries derive low d, those listed as victims derive high d, identity_locked exit_options amplify d toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medieval textual corruption and divergence from classical norms) is live in 1350–1400 — genuine errors in manuscript transmission do exist, and Renaissance scholars genuinely sought to recover authorial intent. By 1500–1600, the founding problem's status shifts toward dead: the major textual reconstruction work is substantially complete, and continued enforcement of classical purity serves institutional coordination and authority consolidation rather than solving ongoing corruption problems. The rupture reading's persistence after the problem attenuates is mandatrophy: the constraint outlives its function. The theater_ratio's rise (0.12 to 0.42) captures this degradation: by 1600, much of the enforcement is performative — writing proper classical Latin is a display of institutional alignment more than a tool for solving textual problems. The measurement series models the transition from functional coordination (early) through extraction-extraction (mid-period) to increasingly theatrical maintenance (late). The coercion grid shows structural pressure building fastest at the organizational level (universities, church institutions) and reaching near-maximum suppression there by 1600, while individual resistance declines. This pattern is consistent with institutional capture: once the constraint is embedded in hiring, curriculum, and publication decisions, individual scholars lack leverage to resist, and the enforcement mechanism maintains itself through routine gatekeeping rather than ongoing persuasion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classical_reconstruction_authenticity,
    'Is the ''classical standard'' reconstructed from ancient texts historically authentic, or does it reflect Renaissance scholars'' normative choices about which texts and usages to privilege?',
    'Comparative historical linguistics analyzing the actual distribution of classical forms in surviving texts vs. the standard humanists established; evidence of Renaissance scholars'' editorial choices in selecting exemplary texts.',
    'If reconstruction is subjective, the rupture reading''s claim to objectivity fails — the standard is human-imposed, not recovered from nature. If reconstruction is largely faithful to ancient usage, the reading''s naturalism claim gains support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classical_reconstruction_authenticity, empirical, 'Whether the classical standard is discovered or constructed.').

omega_variable(
    medieval_corruption_vs_evolution,
    'Does medieval Latin divergence from classical norms constitute corruption, or does it represent legitimate linguistic evolution similar to the classical Latin that evolved from Archaic Latin?',
    'Historical-linguistic analysis of medieval Latin change against principles of natural language evolution; comparison of medieval change to documented evolution in other language traditions; analysis of functional adequacy of medieval innovations for their domains.',
    'If medieval changes are evolutionary and functional, the rupture reading''s corruption framing is revealed as a value judgment, not a factual claim. If medieval changes introduce systematic errors that degrade communicative function, the corruption frame gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_corruption_vs_evolution, empirical, 'Whether medieval Latin divergence is failure or change.').

omega_variable(
    necessity_of_purity_standard,
    'Is the absolute classical purity standard structurally necessary for addressing the founding problem (textual authenticity and corruption detection), or is a less rigid standard sufficient?',
    'Historical analysis of whether medieval and hybrid standards (permitted textual variants, domain-specific norms) would have been adequate for identifying and correcting actual textual errors; comparison of corruption-detection outcomes under different standard regimes.',
    'If purity is not necessary, the standard''s extent of enforcement and the severity of its extraction is indefensible — a more lenient standard would serve coordination without the extractive suppression. If purity is necessary, the extraction is a cost of solving the founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_purity_standard, conceptual, 'Whether the constraint''s scope is minimally sufficient for its function.').

omega_variable(
    identity_locked_medieval_scholars,
    'Are medieval scholars'' embodied Latin practices identity-locked through professional formation, or do they retain capacity to retrain in the classical standard?',
    'Analysis of medieval scholars who attempted classical retraining — success rates, costs, social barriers; age-cohort studies showing whether younger scholars adopted classical norms more readily than older practitioners.',
    'High identity-lock suggests the constraint is more extractive (victims cannot exit) and the suppression is partly internalized (learned practice becomes selfhood). Retrainability suggests exit options are constrained but not foreclosed, moderating the extraction measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_medieval_scholars, empirical, 'Degree of professional identity fusion in medieval Latin practice.').

omega_variable(
    kernel_reading_alternative_framing,
    'Does the rupture reading''s core claim remain coherent if classical texts themselves are recognized as products of social/political choice (which texts were preserved, which suppressed) rather than objective linguistic reality?',
    'Analysis of which classical texts survived and by what mechanism (institutional patronage, copying patterns, accident); evidence of systematic bias in what was transmitted vs. what was lost.',
    'If classical-text preservation is biased selection, the rupture reading''s appeal to textual authority reflects institutional history rather than objective fact. The reading remains live but loses naturalism claims and becomes explicitly value-based rather than discovery-based.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Contingency and construction within the classical-text kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 1350, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1350, latin_correctness__rupture_reading, theater_ratio, 1350, 0.12).
narrative_ontology:measurement_basis(lati_tr_t1350, projected).
narrative_ontology:measurement(lati_tr_t1400, latin_correctness__rupture_reading, theater_ratio, 1400, 0.18).
narrative_ontology:measurement_basis(lati_tr_t1400, observed).
narrative_ontology:measurement(lati_tr_t1450, latin_correctness__rupture_reading, theater_ratio, 1450, 0.28).
narrative_ontology:measurement_basis(lati_tr_t1450, observed).
narrative_ontology:measurement(lati_tr_t1500, latin_correctness__rupture_reading, theater_ratio, 1500, 0.35).
narrative_ontology:measurement_basis(lati_tr_t1500, observed).
narrative_ontology:measurement(lati_tr_t1550, latin_correctness__rupture_reading, theater_ratio, 1550, 0.39).
narrative_ontology:measurement_basis(lati_tr_t1550, observed).
narrative_ontology:measurement(lati_tr_t1600, latin_correctness__rupture_reading, theater_ratio, 1600, 0.42).
narrative_ontology:measurement_basis(lati_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t1350, latin_correctness__rupture_reading, base_extractiveness, 1350, 0.35).
narrative_ontology:measurement_basis(lati_be_t1350, projected).
narrative_ontology:measurement(lati_be_t1400, latin_correctness__rupture_reading, base_extractiveness, 1400, 0.52).
narrative_ontology:measurement_basis(lati_be_t1400, observed).
narrative_ontology:measurement(lati_be_t1450, latin_correctness__rupture_reading, base_extractiveness, 1450, 0.65).
narrative_ontology:measurement_basis(lati_be_t1450, observed).
narrative_ontology:measurement(lati_be_t1500, latin_correctness__rupture_reading, base_extractiveness, 1500, 0.73).
narrative_ontology:measurement_basis(lati_be_t1500, observed).
narrative_ontology:measurement(lati_be_t1550, latin_correctness__rupture_reading, base_extractiveness, 1550, 0.78).
narrative_ontology:measurement_basis(lati_be_t1550, observed).
narrative_ontology:measurement(lati_be_t1600, latin_correctness__rupture_reading, base_extractiveness, 1600, 0.81).
narrative_ontology:measurement_basis(lati_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t1350, latin_correctness__rupture_reading, suppression_requirement, 1350, 0.28).
narrative_ontology:measurement_basis(lati_su_t1350, projected).
narrative_ontology:measurement(lati_su_t1400, latin_correctness__rupture_reading, suppression_requirement, 1400, 0.45).
narrative_ontology:measurement_basis(lati_su_t1400, observed).
narrative_ontology:measurement(lati_su_t1450, latin_correctness__rupture_reading, suppression_requirement, 1450, 0.58).
narrative_ontology:measurement_basis(lati_su_t1450, observed).
narrative_ontology:measurement(lati_su_t1500, latin_correctness__rupture_reading, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement_basis(lati_su_t1500, observed).
narrative_ontology:measurement(lati_su_t1550, latin_correctness__rupture_reading, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement_basis(lati_su_t1550, observed).
narrative_ontology:measurement(lati_su_t1600, latin_correctness__rupture_reading, suppression_requirement, 1600, 0.76).
narrative_ontology:measurement_basis(lati_su_t1600, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1350, tn=1600
narrative_ontology:measurement(lati_grid_01, latin_correctness__rupture_reading, accessibility_collapse(class), 1350, 0.28).
narrative_ontology:measurement(lati_grid_02, latin_correctness__rupture_reading, accessibility_collapse(class), 1600, 0.71).
narrative_ontology:measurement(lati_grid_03, latin_correctness__rupture_reading, accessibility_collapse(individual), 1350, 0.22).
narrative_ontology:measurement(lati_grid_04, latin_correctness__rupture_reading, accessibility_collapse(individual), 1600, 0.72).
narrative_ontology:measurement(lati_grid_05, latin_correctness__rupture_reading, accessibility_collapse(organizational), 1350, 0.31).
narrative_ontology:measurement(lati_grid_06, latin_correctness__rupture_reading, accessibility_collapse(organizational), 1600, 0.78).
narrative_ontology:measurement(lati_grid_07, latin_correctness__rupture_reading, accessibility_collapse(structural), 1350, 0.35).
narrative_ontology:measurement(lati_grid_08, latin_correctness__rupture_reading, accessibility_collapse(structural), 1600, 0.75).
narrative_ontology:measurement(lati_grid_09, latin_correctness__rupture_reading, resistance(class), 1350, 0.64).
narrative_ontology:measurement(lati_grid_10, latin_correctness__rupture_reading, resistance(class), 1600, 0.41).
narrative_ontology:measurement(lati_grid_11, latin_correctness__rupture_reading, resistance(individual), 1350, 0.62).
narrative_ontology:measurement(lati_grid_12, latin_correctness__rupture_reading, resistance(individual), 1600, 0.38).
narrative_ontology:measurement(lati_grid_13, latin_correctness__rupture_reading, resistance(organizational), 1350, 0.58).
narrative_ontology:measurement(lati_grid_14, latin_correctness__rupture_reading, resistance(organizational), 1600, 0.32).
narrative_ontology:measurement(lati_grid_15, latin_correctness__rupture_reading, resistance(structural), 1350, 0.55).
narrative_ontology:measurement(lati_grid_16, latin_correctness__rupture_reading, resistance(structural), 1600, 0.35).
narrative_ontology:measurement(lati_grid_17, latin_correctness__rupture_reading, stakes_inflation(class), 1350, 0.19).
narrative_ontology:measurement(lati_grid_18, latin_correctness__rupture_reading, stakes_inflation(class), 1600, 0.71).
narrative_ontology:measurement(lati_grid_19, latin_correctness__rupture_reading, stakes_inflation(individual), 1350, 0.15).
narrative_ontology:measurement(lati_grid_20, latin_correctness__rupture_reading, stakes_inflation(individual), 1600, 0.68).
narrative_ontology:measurement(lati_grid_21, latin_correctness__rupture_reading, stakes_inflation(organizational), 1350, 0.24).
narrative_ontology:measurement(lati_grid_22, latin_correctness__rupture_reading, stakes_inflation(organizational), 1600, 0.74).
narrative_ontology:measurement(lati_grid_23, latin_correctness__rupture_reading, stakes_inflation(structural), 1350, 0.28).
narrative_ontology:measurement(lati_grid_24, latin_correctness__rupture_reading, stakes_inflation(structural), 1600, 0.76).
narrative_ontology:measurement(lati_grid_25, latin_correctness__rupture_reading, suppression(class), 1350, 0.22).
narrative_ontology:measurement(lati_grid_26, latin_correctness__rupture_reading, suppression(class), 1600, 0.76).
narrative_ontology:measurement(lati_grid_27, latin_correctness__rupture_reading, suppression(individual), 1350, 0.18).
narrative_ontology:measurement(lati_grid_28, latin_correctness__rupture_reading, suppression(individual), 1600, 0.72).
narrative_ontology:measurement(lati_grid_29, latin_correctness__rupture_reading, suppression(organizational), 1350, 0.35).
narrative_ontology:measurement(lati_grid_30, latin_correctness__rupture_reading, suppression(organizational), 1600, 0.81).
narrative_ontology:measurement(lati_grid_31, latin_correctness__rupture_reading, suppression(structural), 1350, 0.31).
narrative_ontology:measurement(lati_grid_32, latin_correctness__rupture_reading, suppression(structural), 1600, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__rupture_reading, 0.08).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The 'latin_correctness' kernel admits three structurally distinct constraint readings: rupture_reading (classical purity, medieval corruption), continuity_reading (medieval as legitimate evolution), and hybrid_reading (domain-dependent standards). Each reading has different ε, beneficiary/victim sets, and temporal dynamics. The rupture reading is the most extractive and the most institutionally dominant by 1600. The continuity reading treats medieval Latin as legitimate and assigns zero or low extraction to medieval practice. The hybrid reading permits classical norms in literary domains while accepting medieval forms in practical domains, creating a mosaic-pattern extraction where technical writers benefit but literary ones face constraint. All three readings share a kernel (the commitment to what constitutes correct Latin) but instantiate fundamentally different constraints in structural terms. This story captures the rupture reading only; the other readings are separate JSON files linked by network.affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
