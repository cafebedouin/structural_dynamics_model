% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Latin Script Adoption as Neutral Phonetic Optimization for Turkish
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This story authors the PHONETIC-INSTRUMENTALIST reading of the 1928
 *   Turkish script reform kernel: the claim that switching from Arabic to
 *   Latin script was a neutral technical decision justified purely by
 *   superior grapheme-phoneme correspondence for Turkish vowel harmony. This
 *   reading is technically well-founded at the phonological level — the
 *   mapping claim is not false — but it is authored here as the reading that
 *   DEPOLITICIZES the decision, treating script as inert encoding technology
 *   and thereby obscuring the same act's function as an instrument of state
 *   identity-construction (severing civic access to the Ottoman-Islamic
 *   textual past). Two sibling constraints exist for the same kernel and are
 *   NOT this file: the kemalist_rupture_reading (which affirmatively claims
 *   the rupture with Ottoman-Islamic continuity AS THE POINT, high ε,
 *   contested legitimacy) and the ottoman_continuity_reading (which holds
 *   Arabic script as constitutive of Turkish-Islamic identity and treats the
 *   reform as extractive loss, high ε, victim-centered). This reading's low ε
 *   reflects its own internal logic — if script really is neutral technology,
 *   there is little to extract — while the theater_ratio climbs over time as
 *   the technical-neutrality framing increasingly does the work of
 *   legitimating an identity-political outcome after the fact, rather than
 *   describing the original decision process.
 *
 * KEY AGENTS:
 *   - turkish_literacy_reformers: Primary agenda-setter (institutional/arbitrage) — administers the transition and frames it as technical
 *   - arabic_script_literate_older_generation: Primary payer (powerless/trapped) — loses civic literacy overnight, absorbed into a narrative of modernization failure rather than policy cost
 *   - religious_education_institutions: Secondary payer (moderate/constrained) — loses general civic standing though retains specialist function
 *   - linguistic_analysts: Analytical observer (analytical/analytical) — can validate the phonetic claim while showing it underdetermines the political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.22).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.58).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Latin Script Adoption as Neutral Phonetic Optimization for Turkish").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__phonetic_instrumentalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, 'f5e81533-b6a1-411d-b36d-501d2c736bdf').
narrative_ontology:cs_kernel_codification('f5e81533-b6a1-411d-b36d-501d2c736bdf', formalized).
narrative_ontology:cs_authority_grounding('f5e81533-b6a1-411d-b36d-501d2c736bdf', expertise).
narrative_ontology:cs_interpretation_layer_present('f5e81533-b6a1-411d-b36d-501d2c736bdf').
narrative_ontology:cs_reading_relation('f5e81533-b6a1-411d-b36d-501d2c736bdf', script_as_identity__ottoman_continuity_reading, influences).
narrative_ontology:cs_reading_relation('f5e81533-b6a1-411d-b36d-501d2c736bdf', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_axiom('f5e81533-b6a1-411d-b36d-501d2c736bdf', foundational, orthographic_choice_is_politically_inert).
narrative_ontology:cs_axiom_status(orthographic_choice_is_politically_inert, holdable).
narrative_ontology:cs_axiom_grounding('f5e81533-b6a1-411d-b36d-501d2c736bdf', orthographic_choice_is_politically_inert, instrumental).
narrative_ontology:cs_axiom('f5e81533-b6a1-411d-b36d-501d2c736bdf', secondary, phonemic_fit_is_the_sole_valid_criterion_for_script_selection).
narrative_ontology:cs_axiom_status(phonemic_fit_is_the_sole_valid_criterion_for_script_selection, holdable).
narrative_ontology:cs_axiom_grounding('f5e81533-b6a1-411d-b36d-501d2c736bdf', phonemic_fit_is_the_sole_valid_criterion_for_script_selection, empirically_contingent).
narrative_ontology:cs_reference_frame('f5e81533-b6a1-411d-b36d-501d2c736bdf', linguistic_engineering_neutrality).
narrative_ontology:cs_drift_state('f5e81533-b6a1-411d-b36d-501d2c736bdf', contemporary_civic_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f5e81533-b6a1-411d-b36d-501d2c736bdf', '').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, turkish_literacy_reformers).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, state_education_apparatus).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, print_publishing_industry).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, arabic_script_literate_older_generation).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, religious_education_institutions).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_archive_readers).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, script_neutrality_doctrine).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, phonetic_transparency_supremacy_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designed and mandated the 1928 script reform, framing it in technical linguistic terms — vowel harmony fit, phoneme-to-grapheme ratio, printing efficiency — while administering a nationwide compulsory re-literacy campaign backed by state penalties for continued Arabic-script use in official and commercial contexts. Their authority to declare the choice 'purely technical' is what lets them avoid naming the identity-severing function of the same policy.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, turkish_literacy_reformers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__phonetic_instrumentalism_reading, turkish_literacy_reformers, beneficiary).

% Gains a single, centrally administrable literacy curriculum and a legible population whose print output the state can monitor and standardize going forward. Frames the transition cost as an investment in a more phonetically transparent system rather than as the production of a discontinuity in who can read what.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, state_education_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Captures a fresh, state-mandated market for new-script textbooks, newspapers, and typesetting equipment. Has commercial incentive to endorse the phonetic-transparency framing since it legitimizes demand for wholesale retooling rather than incremental orthographic reform of the existing script.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, print_publishing_industry, beneficiary,
    organized, biographical, mobile, national).

% Overnight, their acquired literacy becomes functionally obsolete in official, legal, and much commercial life. The technical framing offers them no route back — the choice is presented as a self-evident phonetic upgrade, so their loss reads as failure to modernize rather than as a cost the reform imposed on them. Cannot arbitrage between scripts; must relearn or become functionally illiterate in the new civic order.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, arabic_script_literate_older_generation, payer,
    powerless, biographical, trapped, national).

% Qur'anic and Islamic scholarly instruction is conducted through Arabic script; the reform's 'merely technical' framing does not acknowledge that it severs the ordinary population's script-level access to religious textual tradition, concentrating that access in specialist institutions. Can continue functioning for religious purposes but loses standing in civic and educational life.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, religious_education_institutions, payer,
    moderate, civilizational, constrained, national).

% Future generations of ordinary citizens lose direct script access to five centuries of Ottoman administrative, literary, and legal documents; access becomes the preserve of specially trained paleographers rather than a general civic capacity. The phonetic-transparency framing has no vocabulary for this cost because it treats script purely as an encoding technology, not as an archive key.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_archive_readers, payer,
    powerless, civilizational, trapped, national).

% Evaluate the comparative phonemic fit of Latin versus Arabic orthography for Turkish vowel harmony independent of the political stakes, and can show the technical claim is locally true (Latin with diacritics does map Turkish's eight vowels more transparently than unmodified Arabic script) while also showing that this technical fact underdetermines the political choice of timing, compulsion, and totality of transition.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, linguistic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__phonetic_instrumentalism_reading, state_education_apparatus).
narrative_ontology:fixing_cost_class(script_as_identity__phonetic_instrumentalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine orthographic-fit problem: unmodified Arabic script, built for a Semitic triliteral-root consonantal system, represents Turkish's eight-vowel harmony system poorly, requiring readers to supply vowels from context; a Latin-based alphabet with added diacritics (ç, ş, ğ, ı, ö, ü) can represent each Turkish phoneme with a dedicated grapheme, genuinely lowering ambiguity and easing mass literacy acquisition.
% TRANSFER_FUNCTION: Moves civic legibility and archival access from the Arabic-script-literate population (older generations, religious scholars, Ottoman-era professionals) to the Latin-script-literate population the reform manufactures — while transferring commercial opportunity in publishing and printing to firms positioned to retool, and administrative control over what counts as literacy to the central state.
% ABSENT_VOICES: Arabic-script-literate citizens who lost civic standing overnight were not consulted on the pace or totality of the transition — the decision was announced and enforced within months. Religious authorities who would object that script is not severable from scriptural transmission were structurally outside the reform commission. Their objections would center on cultural-continuity costs the phonetic framing has no category for.
% DISAPPEARANCE_RATIONALE: If the 'script is neutral technology' framing disappeared as the operative justification, the underlying alphabet would likely remain in daily use (retraining a fully literate population back to Arabic script is not costless either), but the political discourse around the 1928 reform would have to directly name its identity-severing function rather than treating it as self-evidently technical — historical memory and civic-education narratives would rearrange even if the orthography itself did not.
% FOUNDING_PROBLEM: Turkish written in unmodified Arabic script underrepresented vowels, slowing literacy acquisition in a population with very low literacy rates in the early 1920s; reformers sought an orthography that would make mass literacy campaigns tractable within a single generation.
% FOUNDING_PROBLEM_CORROBORATION: Independent linguists and literacy historians outside the Turkish state (e.g., comparative Turkic-language phonology scholarship) corroborate that the technical mapping problem was real and that Latin-with-diacritics measurably improved grapheme-phoneme correspondence for Turkish vowels. However, the same scholarship, plus historians of the late Ottoman print culture, note that the *founding problem as stated* — mass illiteracy — no longer exists in any form that would require a script change today, and that the phonetic-instrumentalism framing continues to be invoked in contemporary Turkish civic discourse to depoliticize what participants on multiple sides acknowledge was also a rupture with Ottoman-Islamic textual identity.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, contested).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).
:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because, taken strictly on its own terms, the phonetic-transparency claim describes a genuine, measurable linguistic improvement — Latin-with-diacritics does map Turkish's eight vowels with less ambiguity than unmodified Arabic script, and this is not manufactured. Suppression is moderate-high (0.58) because the reform was compulsory and backed by legal penalty (the 1928 law criminalized use of Arabic script in official documents within a short window), which is suppression in service of the policy regardless of how neutral the stated justification is. Theater ratio starts moderate (0.35) and climbs sharply to 0.62 by the present: at the founding moment the technical argument was doing real persuasive and justificatory work in a live policy debate; over the following century, as the underlying literacy problem was resolved and new generations grew up wholly Latin-script literate, invoking 'script is neutral technology' in retrospective civic discourse increasingly serves to foreclose discussion of the reform's identity-severing consequences rather than to solve any live technical problem — the theater is in the CONTINUED USE of the framing after its original diagnostic function has been discharged.
 *
 * DIRECTIONALITY LOGIC:
 *   Turkish literacy reformers and the state education apparatus sit near the beneficiary end: they administer, are unharmed, and gain centralized legibility and control over what counts as literate civic participation going forward. The print publishing industry benefits commercially from mandated retooling. Arabic-script-literate elders and Ottoman archive readers sit near the target end: trapped exit, no arbitrage between scripts, and a re-narration of their loss as personal failure to modernize rather than a cost the state imposed via compulsion. Religious education institutions occupy a constrained middle position — they retain a functional niche but lose general civic standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (low mass literacy solvable partly through better orthographic fit) is now dead — Turkey has near-universal Latin-script literacy and no live population plausibly benefiting from a return to Arabic-script literacy campaigns. Yet the phonetic-instrumentalism framing persists in civic and educational discourse as the DEFAULT explanation for the reform, functioning less as a live technical justification and more as an inertial narrative that forecloses discussion of the identity-political costs documented in the sibling ottoman_continuity_reading. This is a textbook mandatrophy signature: the mandate (solve an urgent literacy crisis via better phonetic fit) is fully discharged, but the justificatory frame outlives the mandate and now performs a different, undeclared function — depoliticization of a settled identity choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_claim_sufficiency,
    'Does the genuine phonetic superiority of Latin-with-diacritics for Turkish vowel harmony fully justify the SCOPE, PACE, and COMPULSION of the 1928 reform, or does the technical fact only explain a much narrower design choice (which letters to use) while the political choice (whole-population compulsory replacement within months, criminalizing the old script) requires separate justification the phonetic-instrumentalism reading does not supply?',
    'Comparative case analysis: identify other language communities that adopted a technically superior orthography WITHOUT compulsory, rapid, criminalized replacement of the prior script (e.g., gradual orthographic reforms elsewhere), and assess whether comparable phonetic gains were achievable without the identity-severing speed and compulsion actually used in Turkey.',
    'If comparable phonetic gains were achievable via a slower, non-compulsory transition, the phonetic-instrumentalism reading''s ε is understated — the technical claim justifies the letters chosen but not the coercive apparatus used to impose them, and a meaningful share of the reform''s actual function was identity-political rather than technical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_claim_sufficiency, empirical, 'Whether the technical justification covers only the alphabet''s design or also its compulsory, rapid imposition.').

omega_variable(
    script_neutrality_as_kernel_contest_site,
    'Is ''script is neutral technology'' a genuinely separable claim from the other two kernel readings (kemalist_rupture_reading, ottoman_continuity_reading), or is it better understood as the RHETORICAL SURFACE that both the rupture-affirming and continuity-defending camps deploy or contest, meaning the three readings are not independent but are locked in mutual reference?',
    'Discourse analysis of how each reading''s proponents actually use the phonetic-transparency claim — do kemalist_rupture proponents invoke it as a convenient technical cover for an acknowledged political aim, while ottoman_continuity proponents attack it as a fig leaf? If so the three readings are not merely coexisting alternatives but form a single contested rhetorical field.',
    'If the phonetic-instrumentalism reading functions primarily as a rhetorical resource deployed BY the rupture reading rather than as an independently held position, its low ε may understate its true structural role: it would be doing extractive legitimation work on behalf of the rupture reading rather than standing as a neutral third position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_neutrality_as_kernel_contest_site, conceptual, 'Whether the instrumentalist reading is an independent position or a legitimating device serving the rupture reading.').

omega_variable(
    archival_access_externality,
    'Is the loss of general civic script-access to the Ottoman archive (five centuries of administrative, legal, and literary documents) a cost internal to this constraint''s accounting, or an externality this reading''s technical framing structurally cannot register?',
    'Track whether archival-access costs are ever weighed in official retrospective accounts of the reform''s success, versus being addressed only in specialist historiographical literature outside the state''s own narrative.',
    'If the archival-access cost is never weighed by the reading''s own proponents, this confirms the reading''s ε is systematically low BY CONSTRUCTION — the phonetic-transparency frame has no evaluative category for civilizational-timescale archive access, meaning its low-ε finding is an artifact of the frame''s scope, not evidence the cost is small.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(archival_access_externality, conceptual, 'Whether archive-access loss is invisible to this reading by its very framing, not merely undervalued.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 1928, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1928, 0.35).
narrative_ontology:measurement(scri_tr_t1943, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1943, 0.45).
narrative_ontology:measurement(scri_tr_t1960, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1960, 0.52).
narrative_ontology:measurement(scri_tr_t1980, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1980, 0.58).
narrative_ontology:measurement(scri_tr_t2000, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 2000, 0.6).
narrative_ontology:measurement(scri_tr_t2024, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1928, 0.18).
narrative_ontology:measurement(scri_be_t1943, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1943, 0.2).
narrative_ontology:measurement(scri_be_t1960, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1960, 0.21).
narrative_ontology:measurement(scri_be_t1980, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(scri_be_t2000, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(scri_be_t2024, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 2024, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(script_as_identity__phonetic_instrumentalism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.08).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the script_as_identity kernel (the 1928 Turkish alphabet reform). phonetic_instrumentalism_reading (this file) authors low ε on the theory that script choice is technically neutral. kemalist_rupture_reading authors high ε on the theory that severing Ottoman-Islamic continuity was the deliberate and legitimate point of the reform. ottoman_continuity_reading authors high ε on the theory that Arabic script is constitutive of Turkish-Islamic identity and the reform's continuity-severing effect is an extractive loss to those it was imposed upon. All three share the same underlying historical event and stakeholder population but diverge sharply in ε because they diverge in what they take the constraint's TRUE FUNCTION to be — exactly the situation the ε-invariance principle requires resolving via decomposition into separate stories rather than by averaging or hedging within one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
