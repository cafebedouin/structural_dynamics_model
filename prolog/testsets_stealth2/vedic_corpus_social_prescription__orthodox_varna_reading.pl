% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Reading: Divinely Mandated Four-Fold Hierarchy
 *   domain: religious/hermeneutic/social-stratification
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   vedic_corpus_social_prescription: the orthodox_varna_reading, on which
 *   the Vedic corpus (Rigveda 10.90, Bhagavad Gita 4.13, the Dharmashastra
 *   stratum culminating in Manusmriti) literally prescribes a four-fold varna
 *   hierarchy as divinely mandated cosmic order — Brahmin from the mouth,
 *   Kshatriya from the arms, Vaishya from the thighs, Shudra from the feet,
 *   with outcaste populations assigned the order's polluting labor. The
 *   constraint classified here is the standing arrangement that reading
 *   prescribes and maintains: birth-fixed occupation, endogamy, graded ritual
 *   purity, and Shudra/outcaste service obligation, enforced through
 *   sacramental sanction (loss of caste as social death). Its epsilon
 *   referent is that standing arrangement as the orthodox reading itself
 *   frames it: the reading does not deny that labor, fees, and deference flow
 *   upward and that transgression is punished — it sanctifies the flow as
 *   dharma, which is why the reading's own lights still yield high epsilon
 *   over the fixed referent. Sibling readings are separate constraints linked
 *   by network: the reformist_spiritual_reading (spiritual unity,
 *   metaphorical cosmology, no prescriptive social content —
 *   text-attributable epsilon near zero, victim set empty) and the
 *   colonial_orientalist_reading (the corpus as timeless 'Hindu law' codified
 *   for administration — same texts, bureaucratic rather than sacramental
 *   enforcement architecture). The epsilon values differ because the readings
 *   locate the corpus's operative content differently; this file averages
 *   over none of them. KEY AGENTS (by structural relationship): -
 *   brahmin_priestly_class: Agenda-setter and principal beneficiary
 *   (institutional/identity_locked) — interprets the corpus, administers
 *   ritual access, collects fees, grants, and service -
 *   kshatriya_ruling_dynasties: Secondary beneficiary (powerful/constrained)
 *   — purchases legitimation with patronage - vaishya_merchant_castes:
 *   Tertiary beneficiary (moderate/constrained) - shudra_laboring_castes:
 *   Primary target (powerless/trapped) — bears the labor obligation and
 *   ritual disability - dalit_untouchable_communities: Primary target
 *   (powerless/trapped) — bears the order's polluting labor and total
 *   exclusion - anti_caste_movements: Excluded voice (organized) — contests
 *   the mandate from outside the interpretive apparatus -
 *   constitutional_state_india: Observer (institutional/analytical) —
 *   legislates against the order without collecting from it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.68).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.78).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Reading: Divinely Mandated Four-Fold Hierarchy").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/hermeneutic/social-stratification").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, 'c19121a5-240d-43b0-b224-90e00850e7d4').
narrative_ontology:cs_kernel_codification('c19121a5-240d-43b0-b224-90e00850e7d4', fixed_text).
narrative_ontology:cs_authority_grounding('c19121a5-240d-43b0-b224-90e00850e7d4', lineage).
narrative_ontology:cs_interpretation_layer_present('c19121a5-240d-43b0-b224-90e00850e7d4').
narrative_ontology:cs_reading_relation('c19121a5-240d-43b0-b224-90e00850e7d4', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('c19121a5-240d-43b0-b224-90e00850e7d4', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('c19121a5-240d-43b0-b224-90e00850e7d4', foundational, varna_divinely_ordained_inviolable).
narrative_ontology:cs_axiom_status(varna_divinely_ordained_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('c19121a5-240d-43b0-b224-90e00850e7d4', varna_divinely_ordained_inviolable, theological).
narrative_ontology:cs_axiom('c19121a5-240d-43b0-b224-90e00850e7d4', foundational, svadharma_fixed_by_birth_superior_to_universal_ethics).
narrative_ontology:cs_axiom_status(svadharma_fixed_by_birth_superior_to_universal_ethics, holdable).
narrative_ontology:cs_axiom_grounding('c19121a5-240d-43b0-b224-90e00850e7d4', svadharma_fixed_by_birth_superior_to_universal_ethics, theological).
narrative_ontology:cs_reference_frame('c19121a5-240d-43b0-b224-90e00850e7d4', apaurusheya_varna_cosmic_order).
narrative_ontology:cs_drift_state('c19121a5-240d-43b0-b224-90e00850e7d4', contemporary_constitutional_repudiation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c19121a5-240d-43b0-b224-90e00850e7d4', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_dynasties).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_merchant_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_laboring_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_untouchable_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_dynasties).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, karma_rebirth_theodicy).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, purity_pollution_cosmology).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, varnashrama_dharma_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets, transmits, and adjudicates the revealed corpus; alone qualified to recite and teach the Veda, officiate at rites, and declare what the texts require. Receives ritual fees, land grants, and labor service; controls education and textual access. Exit would mean abandoning the status, livelihood, and self-understanding constituted by the office — historically almost unheard of, and the reading itself defines such exit as fall.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, identity_locked, continental).

% Holds military and political power and receives scriptural legitimation of rule in exchange for protecting the priestly order and endowing temples and Brahmin settlements. Dynastic legitimacy is narrated inside the order; ruling against it invites censure and elite defection. Pays for its position through patronage obligations, but the legitimation received has historically exceeded the tribute paid.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_dynasties, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_dynasties, payer).

% Trades and farms under rules that protect property and ritual standing while placing them above the laboring majority. Finances temples and festivals and purchases status through orthodox conformity; stepping outside the order forfeits the standing that secures contracts and marriages.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_merchant_castes, beneficiary,
    moderate, biographical, constrained, regional).

% Bears the agricultural and artisan labor on which the whole order runs, while being barred from Vedic learning, from officiating, and in many codes from accumulating ritual merit; children inherit the same station. Changing trade or marrying out means losing community, marriage prospects, and ritual standing at once, and the texts classify the attempt itself as disorder to be corrected.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_laboring_castes, payer,
    powerless, generational, trapped, continental).

% Sits outside the four varnas yet inside the order's enforcement: assigned the polluting but indispensable labor (scavenging, leatherwork, corpse handling), segregated residentially, denied temple entry, well access, and schooling. Attempts at exit — conversion, migration, simply claiming dignity — have historically met violence, economic boycott, and re-stigmatization that follows the household across generations.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_untouchable_communities, payer,
    powerless, generational, trapped, continental).

% From Buddhist challengers through Bhakti radicals to Ambedkarite Dalit organizers: denies the order's divine mandate and organizes exit, conversion, and equal citizenship. Speaks outside the orthodox interpretive apparatus — barred from the textual conversation by the same educational exclusions it protests — and its objections are answered with censure, counter-mobilization, or violence rather than inclusion.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, anti_caste_movements, excluded,
    organized, biographical, constrained, national).

% Legislates equality, constitutionally abolishes untouchability, reserves offices and seats, and prosecutes caste atrocities — while drawing its electorate and personnel from the same society the order organizes. Takes testimony from every seat and can alter the legal enforcement environment, but collects nothing from the arrangement and cannot by statute reach the social enforcement that replaced the legal kind.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, constitutional_state_india, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates division of labor, ritual access, and marriage regulation across a large agrarian population by assigning each birth-group a fixed occupation, ritual rank, and endogamous boundary, and provides a single cosmological account that integrates the whole allocation into one order.
% TRANSFER_FUNCTION: Moves labor service, ritual fees, agricultural surplus, and deference upward from Shudra and outcaste laboring populations to the Brahmin priesthood (secondarily to allied upper varnas), and moves purity status downward, with each intermediate rank compensated in relative standing over the rank below.
% ABSENT_VOICES: Shudra and Dalit voices were structurally barred from the interpretive conversation — denied Vedic education and the sacred thread, excluded from Sanskrit literacy, and in several codes punished for hearing or reciting scripture. Women of all varnas were likewise excluded from textual authority. Anti-caste dissenters spoke outside the orthodox apparatus and were answered with censure rather than seating; the unanimity of the tradition is in large part the silence of these seats.
% DISAPPEARANCE_RATIONALE: Marriage markets, village labor arrangements, temple economies, residential geography, political mobilization, and kinship networks are all organized around the hierarchy; if it vanished overnight, endogamy would loosen within a generation, labor relations would renegotiate, ritual patronage would redirect, and the entire status economy built on graded purity would have to be rebuilt from scratch.
% FOUNDING_PROBLEM: Integrating the diverse peoples of early agrarian North India into a stable division of labor and a common ritual order: who may officiate, who may fight, who may trade, who must serve, and on what terms royal power and priestly authority mutually legitimate each other.
% FOUNDING_PROBLEM_CORROBORATION: The orthodox acharya tradition attests the founding problem as live, citing enduring needs for social order and ritual structure. Outside the benefiting parties, Dalit intellectual tradition (most systematically Ambedkar's Annihilation of Caste) and mainstream Indological historiography attest that the original integrative problem dissolved with the polities that produced it and that the arrangement persists as domination maintained by its beneficiaries; colonial-era administrative correspondence and epigraphic labor-obligation records corroborate the shifted-function reading from archives the tradition does not control.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end, peaking 0.87 under colonial codification) because the arrangement moves labor service, ritual fees, and deference upward by birth-right, with the rate set by scriptural prescription rather than by any bargain the bearers could renegotiate. Suppression (0.78, raw and unscaled — only extractiveness is scaled by directionality and scope in the engine's computation) reflects the enforcement mix: sacramental sanction, purity policing, endogamy enforcement, and violence against transgressors. Accessibility_collapse is 0.78: once the reading is accepted, alternatives (occupation change, intermarriage, ritual access) collapse almost completely, though costly exits exist — renunciation orders and conversion — which keeps it below natural-law levels. Resistance is 0.68: a continuous multi-millennium counter-tradition (Buddhist challenge, Bhakti radicalism, Lingayat rupture, Ambedkarite conversion and constitutionalism) meets the order everywhere it operates. Theater_ratio crosses 0.5 by 2024: as legal enforcement was dismantled, a growing share of maintenance activity became performative status assertion (ceremonial purity claims, public orthodoxy) decoupled from functioning coordination — a drift signal the engine should weigh, though the extractive core (bonded labor, manual scavenging, endogamy enforcement) remains functional, which is why the claimed type is not revised downward. The temporal series runs on one shared eight-point grid (200, 600, 1000, 1400, 1750, 1900, 1950, 2024) for all three tracked metrics; the 1950 row marks the constitutional discontinuity — legal enforcement machinery dismantled overnight, followed by documented re-consolidation of social enforcement. Receipt surface: gains demonstrably accrue to the Brahmin seat (dakshina schedules, agrahara land grants, corvee entitlements are recorded by recipient), hence the named-seat gain_flow rather than 'diffuse'. Fixing cost is prohibitive on its own evidence: every fix attempt — Buddhist rupture, Bhakti egalitarianism, colonial reform, constitutional abolition, reservation policy — was absorbed, deflected, or answered with backlash, and the seat that could dissolve the arrangement is the seat that constitutes itself through it.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme. From the payer seats (shudra_laboring_castes, dalit_untouchable_communities), the arrangement computes as enforced extraction with no exit; from the agenda-setter seat (brahmin_priestly_class), the identical structure is experienced as sacred duty harmoniously fulfilled — inside the reading, everyone performing birth-assigned dharma looks like order, not extraction. The engine computes this divergence from the structural data; the authored claim does not adjudicate it. Same-level dynamics: shudra and dalit populations share powerlessness but not position — the graded purity scale places each intermediate caste's relative superiority over the caste below as part of its compensation, so horizontal coalition of the extracted is systematically undercut (Ambedkar's graded-inequality analysis); the order pays its middle ranks in status precisely to prevent the coalition a flat two-tier system would face. Inter-institutional dynamics: brahmin interpretive authority and state legal authority operate the same texts differently — the constitutional state dismantled legal enforcement (analytical exit, no collection) while brahminical social enforcement persisted (identity_locked, maximal collection), which is why the post-1950 suppression trough refilled rather than holding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: brahmin_priestly_class sits near the full-beneficiary end (collects fees, grants, service; controls the rules); kshatriya and vaishya seats sit near-beneficiary with modest damping (legitimation and protected commerce against patronage costs); shudra and dalit seats sit near the full-target end (bear the transfer, trapped exit amplifies effective extraction toward the ceiling). The one override: anti_caste_movements (power atom 'organized') are neither declared beneficiaries nor victims, so derivation would fall back to a mid-canonical d; structurally they are targets of the enforcement machinery — they bear violence, boycott, and censure precisely when they act — so d is overridden to 0.8. The constitutional state is an observer with analytical exit; it shapes the enforcement environment without holding a beneficiary or target position.
 *
 * MANDATROPHY ANALYSIS:
 *   The orthodox reading presents the arrangement as eternal divine-cosmic order — a mountain-shaped claim. Declaring its beneficiaries (a seat collecting fees, grants, and labor by birth-right) routes the story through false-summit evaluation: a purported natural/divine order with a concentrated beneficiary is exactly the signature the FSM check exists to catch, and the omegas document the naturality-versus-construction ambiguity the schema requires. Mandatrophy: the founding problem (integrating early agrarian polities, regulating ritual access and labor allocation in Iron Age chiefdoms) is dead in its original form, yet the arrangement persists — the founding_problem_status x disappearance_verdict mismatch flags the zombie dynamic, cross-checked against the rising theater series. The snare classification prevents mislabeling in both directions: it refuses the rope/tangled_rope reading (the genuine coordination content — division of labor, festival integration, meaning provision — is real, but the reading itself criminalizes exit as adharma, which is cover-story structure, not coordination overhead), and it refuses premature piton reclassification (theater above 0.5 notwithstanding, the extractive core still functions and still has victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_position,
    'This constraint is one reading of the kernel vedic_corpus_social_prescription (the orthodox_varna_reading). What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Philological adjudication of the prescriptive status of Rigveda 10.90, Bhagavad Gita 4.13, and the Manusmriti social chapters, plus reception history. If the reformist_spiritual_reading prevails, the text-attributable victim set empties and this constraint''s epsilon collapses toward zero; if the colonial_orientalist_reading prevails, enforcement migrates from sacramental to administrative machinery.',
    'Sibling victory does not tune this file''s metrics — it deletes or re-architects the constraint itself. Reformist adoption removes the victim set entirely (different constraint, near-zero text-attributable epsilon); orientalist adoption replaces priestly enforcement with bureaucratic codification, changing the suppression mechanism while possibly preserving magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_position, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; siblings are separate constraint files, and the dispute is located in the prescriptive status of specific verses.').

omega_variable(
    suppression_structural_internalized_mix,
    'Is the measured suppression structural (residential segregation, economic dependency, violence against transgressors) or internalized (karma-rebirth acceptance of station, purity self-policing, shame carried by the targeted themselves)?',
    'Post-exit trajectory studies: longitudinal comparison of Ambedkarite Buddhist convert communities. If caste stigma, endogamy pressure, and inherited self-pollution beliefs persist after structural exit from the Hindu fold, the internalized share is substantial.',
    'If heavily internalized, effective suppression exceeds the structural measure and persists after legal dismantling — explaining why the post-1950 suppression_requirement trough refilled despite constitutional prohibition. Classification consequence: the constraint survives removal of its enforcement infrastructure longer than a purely structural snare would.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_mix, empirical, 'Structural vs internalized suppression mechanism in caste enforcement.').

omega_variable(
    text_vs_practice_attribution,
    'How much of the measured extraction is caused by the textual prescription itself versus by agrarian social practice that merely invokes textual authority as cover?',
    'Regional-comparative analysis: correlate Brahmin settlement density (agrahara grant clusters), Dharmashastra penetration, and temple-endowment intensity with reconstructed labor-obligation and exclusion gradients across regions and periods.',
    'If practice-driven, the orthodox reading functions as legitimation layer rather than causal engine; epsilon attribution shifts toward the underlying agrarian arrangement and this reading''s own structural culpability drops accordingly. If text-driven, the prescription is the operating mechanism and the reading''s epsilon stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_vs_practice_attribution, empirical, 'Whether the prescription or the practice it clothes carries the extraction.').

omega_variable(
    gain_capture_concentration,
    'Does the arrangement''s gain concentrate in the Brahmin priestly seat (as the gain_flow declaration asserts) or diffuse across all savarna castes?',
    'Quantitative reconstruction of dakshina schedules, land-grant registers, and corvee labor obligations by recipient caste across the epigraphic record.',
    'If gains prove diffuse across upper varnas, gain_flow should read ''diffuse'' and the piton-side receipt cell opens; if concentrated as authored, the snare reading with a named capturing seat stands and the fixing-cost asymmetry follows the Brahmin seat''s stake.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_capture_concentration, empirical, 'Whether extraction receipts capture at the Brahmin seat or diffuse.').

omega_variable(
    historical_metric_provenance,
    'How reliable are the pre-modern points on the temporal series, given reliance on land-grant epigraphy, dharmashastra commentary, and outsider travelogues?',
    'Systematic quantitative epigraphy: code labor-obligation clauses, grant density, and endowment growth as proxy indicators with error bars, rather than point estimates.',
    'Wide error bars could flatten or steepen the accumulation curve and move any computed type-transition dates; the 1950 discontinuity is documentary-solid regardless, since the constitutional moment is directly recorded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_metric_provenance, empirical, 'Provenance confidence limits on pre-modern temporal measurements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 200, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_orthodox_varna_tr_t200, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t600, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 600, 0.22).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t1400, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1400, 0.28).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t1750, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1750, 0.3).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t1900, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1900, 0.38).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(vedic_orthodox_varna_tr_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2024, 0.52).

% Extraction over time
narrative_ontology:measurement(vedic_orthodox_varna_be_t200, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 200, 0.72).
narrative_ontology:measurement(vedic_orthodox_varna_be_t600, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 600, 0.77).
narrative_ontology:measurement(vedic_orthodox_varna_be_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(vedic_orthodox_varna_be_t1400, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1400, 0.82).
narrative_ontology:measurement(vedic_orthodox_varna_be_t1750, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1750, 0.84).
narrative_ontology:measurement(vedic_orthodox_varna_be_t1900, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1900, 0.87).
narrative_ontology:measurement(vedic_orthodox_varna_be_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1950, 0.74).
narrative_ontology:measurement(vedic_orthodox_varna_be_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vedic_orthodox_varna_su_t200, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 200, 0.62).
narrative_ontology:measurement(vedic_orthodox_varna_su_t600, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 600, 0.66).
narrative_ontology:measurement(vedic_orthodox_varna_su_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1000, 0.71).
narrative_ontology:measurement(vedic_orthodox_varna_su_t1400, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1400, 0.75).
narrative_ontology:measurement(vedic_orthodox_varna_su_t1750, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1750, 0.79).
narrative_ontology:measurement(vedic_orthodox_varna_su_t1900, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(vedic_orthodox_varna_su_t1950, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(vedic_orthodox_varna_su_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Vedic varna system' decomposes into three structurally distinct constraints per the epsilon-invariance principle — the orthodox reading (this file: sacramental enforcement, high epsilon, named victims), the reformist spiritual reading (no prescriptive content, near-zero text-attributable epsilon, empty victim set), and the colonial orientalist reading (administrative codification, bureaucratic enforcement). Measuring 'the varna system' through different observables yields different epsilon values because they are different constraints, not one constraint viewed from angles. The upstream orthodox reading supplies the content the orientalist reading codified (historical influence), while the reformist reading directly negates this reading's foundational premise. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
