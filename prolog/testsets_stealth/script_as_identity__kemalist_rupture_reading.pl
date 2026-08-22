% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: State-Mandated Latin Orthography as Civilizational Severance (Kemalist Rupture Reading)
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   In November 1928 the Turkish Republic mandated replacement of the
 *   Arabic-derived Ottoman script with a Latin-based alphabet, banned
 *   Arabic-letter printing, and made the state school system the sole gateway
 *   to literacy. This story instantiates ONE reading of the contested
 *   script_as_identity kernel: the Kemalist rupture reading, for which the
 *   textual severance is not a side effect but the enabling achievement — the
 *   reform works BECAUSE it cuts the population loose from the
 *   Ottoman-Islamic textual past, and the state's monopoly over the literacy
 *   apparatus is what makes the new beginning durable. Per the
 *   epsilon-invariance principle, the sibling readings are separate
 *   constraints with separate files: the ottoman_continuity_reading authors
 *   the same law as destruction of a constitutive identity (higher epsilon,
 *   victim set expanded to the nation's textual memory), and the
 *   phonetic_instrumentalism_reading authors it as neutral technology
 *   adoption (epsilon near the coordination-cost floor). This file's epsilon
 *   is authored over the standing arrangement — the enforced Latin regime and
 *   its accumulated apparatus — assessed by the rupture reading's own lights,
 *   which is why the value sits at moderate-high rather than at the
 *   continuity reading's ceiling.
 *
 * KEY AGENTS:
 *   - kemalist_republican_elite: agenda-setter and primary collector ([institutional]/[arbitrage]) — wrote the law, ran the enforcement, absorbed none of the cost
 *   - secular_state_bureaucracy: secondary beneficiary ([organized]/[mobile]) — staffed and staffed-out of the new literacy apparatus
 *   - latin_script_print_industry: secondary beneficiary ([organized]/[mobile]) — captive market after old-script competition was outlawed
 *   - mass_literate_public: dual-positioned beneficiary-payer ([powerless]/[constrained]) — cheap literacy gained, textual heritage lost
 *   - ottoman_script_literati: primary target ([moderate]/[identity_locked]) — lifetime skill converted to obsolete property
 *   - islamic_clergy_ulema: primary target ([moderate]/[identity_locked]) — interpretive authority severed from lay textual access
 *   - arabic_script_reading_public: primary target ([powerless]/[trapped]) — literate adults made illiterate by fiat
 *   - religious_conservative_opposition: excluded voice ([organized]/[trapped]) — objected with no counting vote and no press
 *   - script_reform_historians: analytical observer ([analytical]/[analytical]) — documents costs the participants did not book
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.45).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.22).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "State-Mandated Latin Orthography as Civilizational Severance (Kemalist Rupture Reading)").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, '34ddde9e-47eb-493f-8b9a-6e90a6f3f298').
narrative_ontology:cs_kernel_codification('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', formalized).
narrative_ontology:cs_authority_grounding('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', extraction).
narrative_ontology:cs_interpretation_layer_present('34ddde9e-47eb-493f-8b9a-6e90a6f3f298').
narrative_ontology:cs_reading_relation('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', foundational, textual_severance_is_emancipation).
narrative_ontology:cs_axiom_status(textual_severance_is_emancipation, holdable).
narrative_ontology:cs_axiom_grounding('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', textual_severance_is_emancipation, instrumental).
narrative_ontology:cs_axiom('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', foundational, republican_future_supersedes_inherited_texts).
narrative_ontology:cs_axiom_status(republican_future_supersedes_inherited_texts, holdable).
narrative_ontology:cs_axiom_grounding('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', republican_future_supersedes_inherited_texts, conventional).
narrative_ontology:cs_reference_frame('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', kemalist_founding_rupture).
narrative_ontology:cs_drift_state('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', contemporary_neo_ottoman_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('34ddde9e-47eb-493f-8b9a-6e90a6f3f298', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_republican_elite).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_state_bureaucracy).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, latin_script_print_industry).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, mass_literate_public).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_script_literati).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, islamic_clergy_ulema).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, arabic_script_reading_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, mass_literate_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and imposed the 1928 Alphabet Law, closed Arabic-script presses, and built the National Schools that taught the new letters. Collected the arrangement's early returns: a bureaucracy staffed by its own graduates, a public sphere reoriented westward, and rivals whose textual authority lapsed overnight. Its members bore no personal cost; their careers ran through the new state, not the old manuscript culture.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_republican_elite, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, kemalist_republican_elite, beneficiary).

% Staffed the literacy campaigns, school system, and inspectorates that carried the reform. The new orthography enlarged the career space of teachers, provincial administrators, and education officials; their skills were minted by the new regime and were worth most inside it.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_state_bureaucracy, beneficiary,
    organized, biographical, mobile, national).

% Retooled from Arabic to Latin typefaces under license and subsidy, then held a captive domestic market once old-script competition was outlawed. Newssheets, textbooks, and state publishing contracts flowed to firms that converted early.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, latin_script_print_industry, beneficiary,
    organized, biographical, mobile, national).

% Learned the new letters in school and gained comparatively cheap, fast literacy plus entry into Latin-alphabet knowledge networks abroad. Carries the flip side indirectly: books, inscriptions, gravestones, and family correspondence from before 1928 are closed to them without specialist training.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, mass_literate_public, beneficiary,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, mass_literate_public, payer).

% Poets, calligraphers, journalists, and senior scribes of the old script. Their craft, income, and standing were bound to Arabic-letter typography and manuscript culture; the ban turned a lifetime of skill into obsolete property almost overnight. Staying inside the tradition meant social marginality; leaving it meant abandoning the body of work that constituted who they were.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_script_literati, payer,
    moderate, biographical, identity_locked, national).

% Interpretive authorities over Qur'an, hadith, and Ottoman jurisprudence. Scripture and commentary stayed in Arabic letters while congregations learned Latin ones, cutting their direct textual address to lay believers and shifting religious mediation toward state-trained clergy. Their authority could not migrate to the new letters without dissolving into something else.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, islamic_clergy_ulema, payer,
    moderate, biographical, identity_locked, national).

% Adults literate in Ottoman Turkish in 1928 who were made illiterate by legal fiat. Some attended the National Schools and relearned; many did not, and spent their remaining years unable to read newspapers, contracts, or their own earlier letters.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, arabic_script_reading_public, payer,
    powerless, biographical, trapped, national).

% Conservative deputies, Sufi orders, and provincial notables who argued the reform uprooted faith and history along with the alphabet. Inside the single-party assembly they held no vote that counted and commanded no press of their own; those who spoke against the bill were answered with closure rather than argument.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_conservative_opposition, excluded,
    organized, biographical, trapped, national).

% Comparative linguists and historians of writing systems who place the Turkish case beside Soviet Latinization, Chinese romanization debates, and later reversals elsewhere. They neither collect nor pay; they document costs the participants did not book and benefits the opponents denied.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, script_reform_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, diffuse).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single phonetically transparent orthography teachable in mass schooling, standardizes spelling across spoken varieties, and plugs Turkish readers into Latin-alphabet technical and commercial networks.
% TRANSFER_FUNCTION: Moves literacy authority and textual access from the Arabic-script learned classes to the state-run school system; moves cultural orientation from the Ottoman-Islamic ecumene toward Western Europe; and moves the pre-1928 textual past itself into an archive most citizens cannot open.
% ABSENT_VOICES: The ulema, the Ottoman literati, and conservative parliamentarians objected but had no counting vote and no open press in the single-party state; Arabic-speaking neighbors whose scriptal ties were severed were never consulted. Their objections survive in memoirs and closed-session records, not in the deliberative record.
% DISAPPEARANCE_RATIONALE: If the Latin mandate and its accumulated apparatus vanished overnight, Turkish schooling, publishing, administration, and digital infrastructure would lose their shared orthography; tens of millions of literate citizens would need retraining, and the state would lose the school-mediated channel through which it has standardized the language since 1928.
% FOUNDING_PROBLEM: Late-Ottoman reformers faced mass illiteracy, an Arabic abjad that maps Turkish vowel harmony poorly, and a governing class that read the empire's stagnation as a civilizational problem requiring a break with the caliphate-era order; the script was chosen as the instrument that would accomplish literacy reform and civilizational reorientation in a single stroke.
% FOUNDING_PROBLEM_CORROBORATION: The literacy half of the founding problem is corroborated from outside the benefiting parties: pre-reform reports by foreign educators and missionaries documented very low Ottoman literacy, and later comparative linguistics concedes the abjad's poor fit for Turkish vowels. The rupture-as-goal half rests chiefly on the reformers' own writings and speeches; religious historians and neo-Ottoman scholars outside the beneficiary set attest that the civilizational-severance aim existed but dispute that it was necessary or legitimate.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).
:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.45 (current) because the standing arrangement still charges a real price — heritage illiteracy, archive inaccessibility, the marginalization of Ottoman-script training — while its steepest charges were levied at founding, which the declining base_extractiveness series records. Suppression is authored LOW (0.22) as a raw structural property of the constraint today: no coercion is needed for compliance, and the residual suppressive element is curricular and archival gatekeeping rather than police action. Suppression is NOT scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Theater is low throughout (0.20) because the literacy function was and is genuine — the National Schools taught real letters to real pupils — though the founding decade's public alphabet lessons mixed pedagogy with pageantry. Accessibility_collapse is 0.55: the Arabic script collapsed as a public medium but survives in religious, academic, and artistic enclaves, so alternatives narrowed sharply without vanishing. Resistance is 0.40, weighting the sharp founding-era opposition (parliamentary dissent, clerical objection, passive noncompliance, all crushed quickly) against present-day quiescence. The three measurement series share one nine-point grid (1928-2026) so every metric is authored at every examined time point; the suppression_requirement series is included because this story specifically traces enforcement-capacity change — a rapid build-up to a 1936 peak, then decay into normalization after the 1950 relaxations, which is the dynamic the scalar alone cannot show.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the agenda-setter seat the arrangement is a founding act it performed and profits from retrospectively legitimizing; from the identity_locked payer seats the same statute reads as confiscation — of craft, income, and textual authority — with no exit that preserves the self. The mass_literate_public seat straddles: it collects the coordination dividend every day and pays the heritage cost invisibly, so its computed position should sit nearer the middle than either pole. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. The elite combines agenda_setter with beneficiary and arbitrage-grade exit, placing it near the full-beneficiary end. Bureaucracy and print industry are clean beneficiaries with mobile exit — low directionality, subsidized by the arrangement. The literati and ulema are payers whose exit is identity_locked: their professional and religious selves were constituted in the old script, so they sit near the full-target end and effective extraction amplifies accordingly. The arabic_script_reading_public is a trapped payer — high directionality without even the consolation of choosing loyalty. The mass public's dual beneficiary/payer declaration yields a mid-range value. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships, and the two same-power pairs (literati vs ulema, both moderate/identity_locked) genuinely share a structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves: mass illiteracy (largely solved within two generations) and civilizational reorientation (achieved, then contested). The coercive mandate's function was transitional — compel the switch until the schooled generation made compulsion redundant — and that function completed around mid-century, hence mandatrophy_resolved is declared true: the mandate outlived its enforcement purpose even though the orthography it installed remains fully load-bearing. Classification discipline matters here in both directions. Calling the arrangement a snare would erase the largest literacy campaign in the region's history and the genuine phonetic fit of the new alphabet; calling it a rope would erase the deliberate dispossession of a learned class and the imposed generational amnesia, which were features of the design, not accidents. Tangled_rope holds both truths: real coordination delivered through a structure that also transferred authority and access asymmetrically. The receipt surface adds a warning the claim does not capture: gain_flow is diffuse and fixing_cost prohibitive, the profile the engine associates with inertial persistence — the temporal series shows the arrangement drifting from actively enforced extraction toward habitual fact, and whether that drift lands it in degraded territory is the engine's computation, not this file's reconciliation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the script_as_identity kernel; how would the sibling readings restructure the classification if instantiated?',
    'Author the sibling stories (ottoman_continuity_reading, phonetic_instrumentalism_reading) with their own beneficiary/victim structures and epsilon values, then compare computed classifications across the family.',
    'The continuity reading would raise epsilon substantially and expand the victim set to the nation''s textual memory as such; the instrumentalism reading would drop epsilon toward the coordination-cost floor and dissolve the victim set into ordinary transition friction. This file''s tangled_rope verdict is conditional on the rupture reading''s framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Classification is indexed to one reading of a three-reading kernel; siblings are separate constraints.').

omega_variable(
    zero_transition_cost_claim,
    'The rupture reading prices the transition at zero cost on the ground that no legitimate incumbents existed to displace; the historical record shows a literate class, a clergy, and a reading public who bore severe losses. Does the zero-cost claim hold only by denying the displaced their standing?',
    'Welfare accounting that counts the displaced classes'' capitalized losses (careers, libraries, authority) alongside the literacy gains, audited from outside the beneficiary set.',
    'If incumbent losses are booked, the reading''s rope-side justification weakens and effective extraction rises; if the denial of standing is accepted, the measured extraction compresses toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_transition_cost_claim, conceptual, 'Whether the reading''s zero-cost premise is an empirical claim or a definitional move.').

omega_variable(
    enforcement_decay_vs_internalization,
    'Is the post-1950 decline in suppression_requirement enforcement decay (the constraint losing its coercive grip) or successful internalization (compliance persisting where enforcement vanished)?',
    'Counterfactual test: compliance held universally in regions and decades where enforcement capacity fell to nil, which indicates internalization rather than decay; survey and archival evidence on voluntary old-script use fills the remainder.',
    'Decay would mark the arrangement as reversible and its residual extraction as contingent; internalization would mark the orthography as settled convention, converting ongoing extraction into a historical stock rather than a continuing flow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_decay_vs_internalization, empirical, 'Whether falling suppression reflects weakening or completed normalization.').

omega_variable(
    heritage_severance_cost_attribution,
    'Is the persisting inability of ordinary citizens to read pre-1928 texts an extraction imposed by this constraint, or an ordinary and unavoidable cost of any successful orthographic standardization?',
    'Cross-case comparison with polities that retained or restored older scripts (Japan''s kanji retention costs, Greek diglossia resolution, post-Soviet script politics in Central Asia) to separate standardization''s intrinsic price from the Turkish case''s deliberate archival severance.',
    'If the severance is attributable to design choices beyond standardization''s needs, residual extraction supports the tangled_rope verdict indefinitely; if it is intrinsic cost, the arrangement''s current epsilon falls toward rope territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heritage_severance_cost_attribution, conceptual, 'Attribution of the standing heritage-access cost to the constraint versus to standardization as such.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 1928, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__kemalist_rupture_reading, theater_ratio, 1928, 0.3).
narrative_ontology:measurement(scri_tr_t1936, script_as_identity__kemalist_rupture_reading, theater_ratio, 1936, 0.28).
narrative_ontology:measurement(scri_tr_t1946, script_as_identity__kemalist_rupture_reading, theater_ratio, 1946, 0.26).
narrative_ontology:measurement(scri_tr_t1956, script_as_identity__kemalist_rupture_reading, theater_ratio, 1956, 0.24).
narrative_ontology:measurement(scri_tr_t1966, script_as_identity__kemalist_rupture_reading, theater_ratio, 1966, 0.23).
narrative_ontology:measurement(scri_tr_t1980, script_as_identity__kemalist_rupture_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(scri_tr_t1995, script_as_identity__kemalist_rupture_reading, theater_ratio, 1995, 0.21).
narrative_ontology:measurement(scri_tr_t2010, script_as_identity__kemalist_rupture_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(scri_tr_t2026, script_as_identity__kemalist_rupture_reading, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1928, 0.72).
narrative_ontology:measurement(scri_be_t1936, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1936, 0.68).
narrative_ontology:measurement(scri_be_t1946, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1946, 0.63).
narrative_ontology:measurement(scri_be_t1956, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1956, 0.58).
narrative_ontology:measurement(scri_be_t1966, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1966, 0.54).
narrative_ontology:measurement(scri_be_t1980, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(scri_be_t1995, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1995, 0.47).
narrative_ontology:measurement(scri_be_t2010, script_as_identity__kemalist_rupture_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement(scri_be_t2026, script_as_identity__kemalist_rupture_reading, base_extractiveness, 2026, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1928, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1928, 0.75).
narrative_ontology:measurement(scri_su_t1936, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1936, 0.78).
narrative_ontology:measurement(scri_su_t1946, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1946, 0.72).
narrative_ontology:measurement(scri_su_t1956, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1956, 0.55).
narrative_ontology:measurement(scri_su_t1966, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1966, 0.42).
narrative_ontology:measurement(scri_su_t1980, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1980, 0.33).
narrative_ontology:measurement(scri_su_t1995, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(scri_su_t2010, script_as_identity__kemalist_rupture_reading, suppression_requirement, 2010, 0.24).
narrative_ontology:measurement(scri_su_t2026, script_as_identity__kemalist_rupture_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the Turkish script reform' decomposes into three structurally distinct constraints corresponding to the three readings of the script_as_identity kernel. This file (kemalist_rupture_reading) authors the enforced Latin regime as emancipatory severance with moderate-high epsilon; ottoman_continuity_reading authors the same statute as identity destruction with a high epsilon and an expanded victim set; phonetic_instrumentalism_reading authors it as technology adoption with epsilon near the coordination floor. The upstream member is phonetic_instrumentalism_reading (the phonetic-fit claim is the most empirically settled and is cited as justification by the other two); the rupture reading mediates between it and the continuity reading, which is downstream in the sense that continuity discourse defines itself against the rupture's achievements. Each file links the others via affects_constraints; epsilon divergence across the family is the measured quantity, not an inconsistency to be reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
