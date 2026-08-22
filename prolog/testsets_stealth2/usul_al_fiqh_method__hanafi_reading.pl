% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Reading of Usul al-Fiqh: Expansive Analogical Method with Juristic Preference
 *   domain: religious/legal/jurisprudential
 *
 * SUMMARY:
 *   Within Sunni legal theory the kernel 'by what procedure is law derived
 *   from revelation' stabilized into four school-readings. This story authors
 *   the Hanafi reading as a clean, epsilon-invariant constraint: qiyas
 *   expansively applicable wherever the texts are silent, ra'y supplementing
 *   where analogy fails, istihsan permitting departure from strict analogy
 *   for public interest. The epsilon referent is the standing Hanafi
 *   methodological arrangement itself — formed in Abu Hanifa's circle,
 *   codified by his disciples, institutionalized in madrasa and court, raised
 *   to official-school status under imperial patronage, and contracted by
 *   nineteenth-century codification — assessed from the analytical seat of
 *   this authoring model. The manifest's declared victim ('textualist claim
 *   to limit innovation') is operationalized as the actor class bearing the
 *   arrangement's costs, hadith_textualist_scholars, because a claim collects
 *   no rents and belongs under vindicated_propositions instead; the
 *   arrangement vindicates analogical extension via operative cause and
 *   public-interest departure. Family links run to all three sibling readings
 *   via network.affects_constraints, and the dual-formulation note records
 *   the decomposition.
 *
 * KEY AGENTS:
 *   - hanafi_jurist_class: primary beneficiary and method administrator (organized/identity_locked) — collects derivational authority, office, income, and standing
 *   - hadith_textualist_scholars: primary target (organized/constrained) — bears displacement of the textualist claim to bind innovation
 *   - imperial_legal_administrations: enforcing patron and secondary beneficiary (institutional/arbitrage) — enforces official-school status, collects administrable law
 *   - lay_litigants_in_hanafi_courts: diffuse payer with incidental benefit (powerless/constrained) — receives adaptive law it cannot audit
 *   - rival_madhhab_jurists: excluded competitors (organized/constrained) — barred from official application inside Hanafi jurisdictions
 *   - comparative_law_historians: analytical observer — sees the full four-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.4).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Reading of Usul al-Fiqh: Expansive Analogical Method with Juristic Preference").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "religious/legal/jurisprudential").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '21fed18d-1bb7-4c38-94ef-6cc27ad04047').
narrative_ontology:cs_kernel_codification('21fed18d-1bb7-4c38-94ef-6cc27ad04047', fixed_text).
narrative_ontology:cs_authority_grounding('21fed18d-1bb7-4c38-94ef-6cc27ad04047', lineage).
narrative_ontology:cs_interpretation_layer_present('21fed18d-1bb7-4c38-94ef-6cc27ad04047').
narrative_ontology:cs_reading_relation('21fed18d-1bb7-4c38-94ef-6cc27ad04047', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('21fed18d-1bb7-4c38-94ef-6cc27ad04047', usul_al_fiqh_method__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('21fed18d-1bb7-4c38-94ef-6cc27ad04047', usul_al_fiqh_method__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('21fed18d-1bb7-4c38-94ef-6cc27ad04047', foundational, expansive_qiyas_in_textual_silence).
narrative_ontology:cs_axiom_status(expansive_qiyas_in_textual_silence, holdable).
narrative_ontology:cs_axiom_grounding('21fed18d-1bb7-4c38-94ef-6cc27ad04047', expansive_qiyas_in_textual_silence, theological).
narrative_ontology:cs_axiom('21fed18d-1bb7-4c38-94ef-6cc27ad04047', foundational, istihsan_valid_public_interest_departure).
narrative_ontology:cs_axiom_status(istihsan_valid_public_interest_departure, holdable).
narrative_ontology:cs_axiom_grounding('21fed18d-1bb7-4c38-94ef-6cc27ad04047', istihsan_valid_public_interest_departure, instrumental).
narrative_ontology:cs_axiom('21fed18d-1bb7-4c38-94ef-6cc27ad04047', secondary, ray_supplements_where_analogy_fails).
narrative_ontology:cs_axiom_status(ray_supplements_where_analogy_fails, holdable).
narrative_ontology:cs_axiom_grounding('21fed18d-1bb7-4c38-94ef-6cc27ad04047', ray_supplements_where_analogy_fails, conventional).
narrative_ontology:cs_reference_frame('21fed18d-1bb7-4c38-94ef-6cc27ad04047', revelation_extended_through_disciplined_reason).
narrative_ontology:cs_drift_state('21fed18d-1bb7-4c38-94ef-6cc27ad04047', post_codification_statute_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('21fed18d-1bb7-4c38-94ef-6cc27ad04047', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, imperial_legal_administrations).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, hadith_textualist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, lay_litigants_in_hanafi_courts).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_litigants_in_hanafi_courts).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, analogical_extension_via_operative_cause).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, public_interest_departure_from_strict_analogy).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, continuity_of_revelation_through_disciplined_reason).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains in the school's method through madrasa curricula, issues fatwas and decides cases by qiyas, ra'y, and istihsan, staffs the judgeships and muftiates of Hanafi-governed territories, and reproduces itself through master-disciple transmission licenses. Its scholarly standing, livelihood, and law-making role all depend on the method's latitude: each expansion of analogical or preference-based derivation enlarges the domain governed by its judgment rather than by text alone. Leaving the method would mean forfeiting the credentials, offices, and epistemic identity built on it.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, agenda_setter).

% Guard the position that only explicit text and authenticated report bind the community, and that juristic preference is legislation rather than derivation. Under Hanafi institutional dominance their method is demoted: courts apply Hanafi doctrine, their objections register as inter-school polemic rather than binding constraint, and their students face curricula that teach analogy before hadith criticism. Exit would mean abandoning the textualist project they define themselves by; staying means permanent minority status inside Hanafi jurisdictions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hadith_textualist_scholars, payer,
    organized, generational, constrained, continental).

% Appoints chief judges and muftis from the Hanafi school, incorporates its doctrine into court procedure and, in the Ottoman case, into the kanun alongside siyasa statutes, and enforces official-school status by passing over rival-method candidates. Gains a body of law flexible enough to absorb fiscal, commercial, and administrative novelty without amending sacred texts; pays in enforcement expenditure and in ceding day-to-day law-shaping to the jurist class. Dynasties have shifted patronage among schools before, so rerouting official status is available at the margin.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, imperial_legal_administrations, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, imperial_legal_administrations, beneficiary).

% Bring disputes to courts that decide by the school's method and receive rulings whose chain of justification runs through juristic analogy and preference rather than verse or report they could consult directly. They gain law that adapts to forms of commerce, marriage, and property the texts never contemplated, and they bear the cost of being unable to audit a ruling's basis without the jurist class's training; where istihsan departs from strict analogy, the outcome tracks the jurist's equity judgment rather than a rule they could have anticipated.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_litigants_in_hanafi_courts, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, lay_litigants_in_hanafi_courts, beneficiary).

% Maliki, Shafi'i, and Hanbali jurists maintain parallel methods and, where their own schools hold official status, mirror-image arrangements. Inside Hanafi-administered territories they are passed over for official judgeships and their method is barred from court application; they contest the Hanafi latitude in polemic and in their own academies but hold no seat in the methodological conversation that fixes the law they live under when resident in its jurisdictions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rival_madhhab_jurists, excluded,
    organized, generational, constrained, global).

% Trace how the four readings allocate authority between text and jurist reason, reconstruct the drift from juristic discretion to codified statute, and compare the Hanafi arrangement's extraction profile against its siblings'. Hold no stake in any school's standing and collect nothing from its operation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_law_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends a finite revealed corpus to an unbounded stream of novel cases: qiyas carries an established ruling to a new case through a shared operative cause, ra'y supplies reasoned judgment where no analogy holds, and istihsan adjusts strict analogy when it would defeat the law's evident purposes — producing consistent, continuous adjudication without waiting for new revelation.
% TRANSFER_FUNCTION: Moves derivational authority — and the judicial office, teaching chairs, fee income, and social standing attached to it — from the texts and their textualist guardians to the rationalist-trained jurist class; every qiyas-based or preference-based ruling is an exercise of that transferred authority, and madrasa licensure perpetuates the transfer across generations.
% ABSENT_VOICES: Hadith-textualist scholars objected continuously — al-Shafi'i's polemic against istihsan, Zahiri rejection of qiyas altogether — but inside Hanafi-administered courts their objections carried no procedural weight. Lay litigants subject to preference-based rulings had no seat in methodological councils. Women, whose family-law outcomes istihsan frequently shaped, were absent from the derivational debate entirely.
% DISAPPEARANCE_RATIONALE: Overnight loss would strand every novel case in Hanafi jurisdictions without a derivation procedure, void the legitimacy chain of centuries of issued rulings, dissolve the jurist class's function and the state's supply of adaptable law, and leave the field to whichever rival method could organize fastest — the entire Hanafi legal economy rearranges around the loss.
% FOUNDING_PROBLEM: The Quran and Sunna are finite; the cases a trading civilization's courts confront are not. The arrangement was built to solve how revealed law can reach cases the texts never addressed without either suspending the law or admitting unconstrained human legislation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: al-Shafi'i's al-Risala concedes the novelty problem while denying the Hanafi solution, and the Zahiri and Hanbali programs presuppose the same problem with different answers; modern comparative-jurisprudence scholarship treats source-extension as the constitutive problem of every revealed-law system. No serious participant, including the method's fiercest opponents, disputes that the founding problem exists — only whether this arrangement solves it legitimately.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.68 is authored for the standing Hanafi arrangement itself, not for any rival reading's arrangement. Extraction is substantial but bounded: the method transfers derivational authority to a credentialed class whose rulings govern populations unable to verify their bases without the same training, and the class collects office, income, and standing from the transfer; it is bounded by ijma's cap on unilateral innovation, the 'illa discipline on analogy, and the fact that the coordination output — novel-case coverage — is genuinely delivered. Suppression 0.40 is a raw structural property, unscaled by power or scope: it reflects institutional exclusion of rival methods inside Hanafi-administered courts (appointment control, curriculum control), not coercive suppression of belief, and it decays after the caliphate's abolition. Theater 0.35: the framing of qiyas and istihsan as discovery-from-revelation rather than juristic legislation sustains divine legitimacy for what is substantially human lawmaking; the derivation machinery itself remains functional, so theater stays a minority share. Accessibility collapse 0.40: understanding the method does not close alternatives — three sibling readings and direct textualism persisted alongside it for twelve centuries; closure was jurisdictional, not cognitive. Resistance 0.60: sustained textualist polemic, al-Shafi'i's campaign against istihsan, Zahiri rejection of qiyas altogether, and repeated intra-school attempts to narrow istihsan. The temporal series runs on one shared grid (750–1926, eight points, all three metrics at every point): extraction and theater climb with institutionalization and imperial patronage, peaking at the Ottoman apex, then ease as codification converts juristic derivation into statutory text. The claimed type (tangled_rope) is asserted from structure — genuine coordination function plus asymmetric capture plus active enforcement — independently of these metric values; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The jurist seat computes a stewardship arrangement it built and reproduces; the textualist seat computes displacement of its claim to bind innovation; the lay-litigant seat computes adaptive law it cannot audit. Identity-lock sharpens the divergence: the jurist class's exit is identity_locked in the professional-institutional sense — the school's method is not a tool its members hold but the constitution of their epistemic standing; a Hanafi master who accepted textualist primacy would not relocate, he would dissolve. Were that frame to break — as codification partially broke it — the class's directionality would migrate toward the target end as its collected authority became contestable. Same-level dynamics: the jurist class and the textualist scholars hold comparable organized power at the civilizational scale, yet experience opposite directionalities; the differentiator is jurisdictional incumbency, not global standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. hanafi_jurist_class (beneficiary, identity_locked) sits near the subsidized end — identity-locked beneficiaries sit nearer the beneficiary pole than mobile ones. imperial_legal_administrations declares beneficiary, but the derivation from that declaration alone would understate its position: it funds enforcement, cedes day-to-day law-shaping to the jurist class, and carries the legitimacy risk of discretionary rulings, so a directionality override sets the institutional atom to d=0.35, near-symmetric. hadith_textualist_scholars (victim, constrained) sits near the full-target end. lay_litigants_in_hanafi_courts (payer with secondary beneficiary, powerless, constrained) computes elevated but below-textualist: they bear unauditable-discretion costs while receiving adaptive law. rival_madhhab_jurists are excluded rather than coordinated — their exclusion is part of what the enforcement machinery maintains. The observer seat collects nothing and pays nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — finite texts, unbounded cases — is live, so the arrangement cannot be dismissed as mandate surviving function; mandatrophy_resolved is deliberately not declared. Conversely the receipt surface blocks the opposite error: gains demonstrably accrue to the jurist class (named in gain_flow), so the arrangement cannot pass as pure rope despite its real coordination output. The R5 mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges — no zombie flag fires. The genuine obsolescence risk sits at the interval's end: codification converted juristic derivation into statutory text, and the omega post_codification_vestige_status tracks whether the method now runs as living derivation or inherited credential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (hanafi_reading) of the usul_al_fiqh_method kernel; would instantiating the maliki, shafii, or hanbali readings yield different epsilon and classification?',
    'Compile the three sibling stories with their own beneficiary/victim structures and compare computed per-seat types and chi profiles across the family.',
    'The hanbali reading (minimal qiyas, weak-hadith preference) should compute materially lower jurist-capture and possibly a rope-side classification; the shafii reading trades istihsan latitude for hadith-criticism gatekeeping, shifting who extracts. Only the family comparison, not this file alone, establishes whether the colloquial label ''usul al-fiqh'' covers a spread of structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one-of-four kernel reading; the disagreement is located in the allocation of derivational authority between text and jurist reason.').

omega_variable(
    discovery_legitimacy_cover,
    'Is the framing of qiyas and istihsan as discovery-from-revelation a good-faith epistemology of extension, or legitimacy cover for juristic legislation?',
    'Internal school evidence: the Hanafi masters'' own characterizations of istihsan as preference rather than derivation, comparison of issued rulings where istihsan contradicts strict analogy, and the reception history of al-Shafi''i''s critique inside Hanafi institutions.',
    'If cover, the theater_ratio understates the arrangement''s performative share and extraction includes legitimacy rents; if good faith, part of the measured extraction is the price of the coordination itself and the rope-side accounting gains weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovery_legitimacy_cover, conceptual, 'Whether the discovery framing is epistemology or legitimacy maintenance.').

omega_variable(
    istihsan_frequency_materiality,
    'How often did istihsan actually overturn strict-analogy outcomes in practice, as opposed to confirming them with added justification?',
    'Corpus analysis of fatwa collections and court records coding departure frequency, the stated public-interest grounds, and whether the strict-analogy outcome was available and rejected.',
    'Low departure frequency concentrates the extraction in the authority claim rather than outcomes; high frequency means substantive lawmaking by preference, raising effective epsilon and strengthening the tangled-rope reading against rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_frequency_materiality, empirical, 'Material rate of preference-based departure from strict analogy.').

omega_variable(
    post_codification_vestige_status,
    'After Mecelle-style codification and the caliphate''s abolition, does the method operate as living derivation or as an inherited credential in personal-status courts and informal muftiate practice?',
    'Survey contemporary Hanafi muftiate and personal-status court practice: whether rulings cite qiyas chains and istihsan reasoning or statutory text, and what madrasa curricula emphasize.',
    'A vestige finding pushes successor institutions toward piton-flavored drift with rising theater; a living-method finding preserves the tangled-rope profile and keeps the jurist class''s extraction current rather than residual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_codification_vestige_status, empirical, 'Living method versus inherited credential after codification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 750, 1926).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t750, usul_al_fiqh_method__hanafi_reading, theater_ratio, 750, 0.18).
narrative_ontology:measurement_basis(usul_tr_t750, observed).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__hanafi_reading, theater_ratio, 900, 0.24).
narrative_ontology:measurement_basis(usul_tr_t900, observed).
narrative_ontology:measurement(usul_tr_t1050, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1050, 0.29).
narrative_ontology:measurement_basis(usul_tr_t1050, observed).
narrative_ontology:measurement(usul_tr_t1250, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1250, 0.33).
narrative_ontology:measurement_basis(usul_tr_t1250, observed).
narrative_ontology:measurement(usul_tr_t1450, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1450, 0.37).
narrative_ontology:measurement_basis(usul_tr_t1450, observed).
narrative_ontology:measurement(usul_tr_t1600, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1600, 0.41).
narrative_ontology:measurement_basis(usul_tr_t1600, observed).
narrative_ontology:measurement(usul_tr_t1750, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1750, 0.39).
narrative_ontology:measurement_basis(usul_tr_t1750, observed).
narrative_ontology:measurement(usul_tr_t1926, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1926, 0.35).
narrative_ontology:measurement_basis(usul_tr_t1926, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t750, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 750, 0.45).
narrative_ontology:measurement_basis(usul_be_t750, observed).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 900, 0.54).
narrative_ontology:measurement_basis(usul_be_t900, observed).
narrative_ontology:measurement(usul_be_t1050, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1050, 0.6).
narrative_ontology:measurement_basis(usul_be_t1050, observed).
narrative_ontology:measurement(usul_be_t1250, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1250, 0.64).
narrative_ontology:measurement_basis(usul_be_t1250, observed).
narrative_ontology:measurement(usul_be_t1450, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1450, 0.69).
narrative_ontology:measurement_basis(usul_be_t1450, observed).
narrative_ontology:measurement(usul_be_t1600, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1600, 0.72).
narrative_ontology:measurement_basis(usul_be_t1600, observed).
narrative_ontology:measurement(usul_be_t1750, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1750, 0.71).
narrative_ontology:measurement_basis(usul_be_t1750, observed).
narrative_ontology:measurement(usul_be_t1926, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1926, 0.68).
narrative_ontology:measurement_basis(usul_be_t1926, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t750, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 750, 0.22).
narrative_ontology:measurement_basis(usul_su_t750, observed).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 900, 0.32).
narrative_ontology:measurement_basis(usul_su_t900, observed).
narrative_ontology:measurement(usul_su_t1050, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1050, 0.44).
narrative_ontology:measurement_basis(usul_su_t1050, observed).
narrative_ontology:measurement(usul_su_t1250, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1250, 0.52).
narrative_ontology:measurement_basis(usul_su_t1250, observed).
narrative_ontology:measurement(usul_su_t1450, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1450, 0.58).
narrative_ontology:measurement_basis(usul_su_t1450, observed).
narrative_ontology:measurement(usul_su_t1600, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1600, 0.63).
narrative_ontology:measurement_basis(usul_su_t1600, observed).
narrative_ontology:measurement(usul_su_t1750, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement_basis(usul_su_t1750, observed).
narrative_ontology:measurement(usul_su_t1926, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1926, 0.4).
narrative_ontology:measurement_basis(usul_su_t1926, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'usul al-fiqh method': the label conflates four structurally distinct arrangements that allocate derivational authority differently, so each reading is authored as its own story with its own epsilon, beneficiary/victim structure, and classification, linked by network.affects_constraints. The Hanafi reading defines the maximum-latitude pole of the family; the shafii_reading systematized the meta-discipline all four use for self-presentation (upstream influence running from shafii_reading into its siblings' institutional form), while the hanbali_reading defines the minimum-latitude pole. Epsilon differs across the family because the jurist-capture surface scales with granted latitude: the hanbali reading should compute materially lower jurist-class extraction, the maliki reading substitutes Medinan-practice and maslaha capture for analogical capture, and the shafii reading shifts extraction toward hadith-criticism gatekeeping.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanafi_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
