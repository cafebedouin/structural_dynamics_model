% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections - Expansive Reading (Functional Continuity Guarantee)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the expansive reading of the 1923 Treaty of
 *   Lausanne's minority protections (Articles 37-45): the guarantee of
 *   functional continuity of pre-1923 religious governance - institutional
 *   self-administration, property rights, and clergy formation through
 *   theological schools - for the recognized non-Muslim communities of
 *   Turkey, mirrored for the Muslims of Western Thrace. The standing
 *   arrangement the story is about is the guarantee regime as it has actually
 *   operated since 1923, assessed by this reading's own lights: a reciprocal
 *   treaty compact that transfers institutional authority to the communities,
 *   requires continuous external enforcement to hold against
 *   administering-state drift, and leaves the communities' institutional
 *   survival contingent on compliance cycles they do not control. Epsilon's
 *   referent is that standing arrangement - not the fully honored arrangement
 *   this reading demands. The claim (rope) and the metrics are authored
 *   independently: the metrics record a century of enforcement decay,
 *   mid-century catastrophe, and partial post-2008 restoration, including an
 *   elevated theater ratio and a diffuse-gain/prohibitive-fix receipt profile
 *   that the engine weighs against the claim. Sibling readings of the same
 *   kernel text are separate constraint stories with their own epsilon
 *   values; this file authors only the expansive reading's constraint.
 *
 * KEY AGENTS:
 *   - ecumenical_patriarchate_and_rum_institutions: primary beneficiary (organized/identity_locked) - collects institutional continuity; its clergy pipeline is the guarantee's most visibly broken limb
 *   - armenian_patriarchate_and_community: beneficiary (organized/identity_locked) - continuity of schools and foundations under state-administered procedures
 *   - jewish_community_of_turkey: beneficiary (organized/constrained) - protected institutions sustained by a remnant population after mass exit
 *   - western_thrace_muslim_community: mirror-side beneficiary (organized/constrained) - the reciprocal seat that disciplines both administrations
 *   - turkish_state: administering party (institutional/constrained) - controls domestic delivery; bears sovereignty costs; converts implementation discretion into bilateral leverage
 *   - lausanne_guarantor_states: enforcement beneficiary (institutional/constrained) - collects supervision leverage; enforcement effort highly variable across the century
 *   - european_human_rights_mechanisms: analytical observer (institutional/analytical) - adjudicates cases and grades compliance; accumulates the operative interpretation
 *   - unrecognized_religious_communities: excluded voice (powerless/constrained) - enumerated out of the protected triad
 *   - imvros_greek_islanders: excluded voice (powerless/trapped) - grievances outside the classic frame until late European-court intervention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.32).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.45).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections - Expansive Reading (Functional Continuity Guarantee)").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b').
narrative_ontology:cs_kernel_codification('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', fixed_text).
narrative_ontology:cs_authority_grounding('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', distributed).
narrative_ontology:cs_reading_relation('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', foundational, institutional_functional_continuity_guaranteed).
narrative_ontology:cs_axiom_status(institutional_functional_continuity_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', institutional_functional_continuity_guaranteed, conventional).
narrative_ontology:cs_axiom('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', secondary, corporate_continuity_as_protection_unit).
narrative_ontology:cs_axiom_status(corporate_continuity_as_protection_unit, holdable).
narrative_ontology:cs_axiom_grounding('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', corporate_continuity_as_protection_unit, deontological).
narrative_ontology:cs_reference_frame('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', functional_continuity_compact).
narrative_ontology:cs_drift_state('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('93d22f2f-7fdb-4fd0-8f70-3f6fb82d872b', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, ecumenical_patriarchate_and_rum_institutions).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_patriarchate_and_community).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, jewish_community_of_turkey).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, western_thrace_muslim_community).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, lausanne_guarantor_states).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, treaty_reciprocity_guarantee_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, institutional_functional_rights_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Greek Orthodox institutions of Istanbul - the Patriarchate, parish schools, hospitals, and foundations - under the treaty's guarantee of institutional continuity. What flows to it: legal footing for self-administration, property claims, and clergy formation predating the republic. What flows from it: daily operation of worship and schooling for a community shrunk from over one hundred thousand to a few thousand. Exit looks like dissolution: relocating the See or dispersing the institutions ends fifteen centuries of uninterrupted continuity, so the community's fate and the institutions' survival are the same question. Its clergy pipeline has been severed since the Halki seminary's closure in 1971, leaving each ordination a draw on a diminishing stock without renewal.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, ecumenical_patriarchate_and_rum_institutions, beneficiary,
    organized, civilizational, identity_locked, global).

% Runs the Armenian Apostolic Patriarchate's churches, schools, and foundations in Turkey under the same guarantee. Collects recognized status for its schools and charitable institutions; its institutional life contracted sharply after 1915 and again through mid-century expropriations. Foundation elections and property registrations proceed through state-administered procedures it does not control. Exit would mean folding the patriarchate's functions into diaspora bodies and ending the seat's continuity on its historic territory.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, armenian_patriarchate_and_community, beneficiary,
    organized, generational, identity_locked, national).

% Maintains synagogues, schools, and the Chief Rabbinate under the treaty's recognition. Collects protected status for its institutions; paid for it historically through near-total emigration - the remaining few thousand members sustain institutions sized for a population many times larger. Its exit option (emigration) is real and was exercised by most of the community, so those who remain have chosen continuity under the guarantee rather than being unable to leave.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, jewish_community_of_turkey, beneficiary,
    organized, biographical, constrained, national).

% The mirror side of the reciprocal bargain: Muslims of Western Thrace in Greece hold parallel guarantees for muftiates, waqfs, and minority schools. Collects the same institutional continuity the treaty secures for the communities in Turkey; its situation disciplines both administrations, since each government's compliance is read against the other's. Disputes over appointed versus elected muftis and waqf administration recur, but the guarantee's existence anchors every negotiation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, western_thrace_muslim_community, beneficiary,
    organized, generational, constrained, regional).

% Administers the protections domestically: it registers minority foundations, licenses minority schools, decides which institutions receive legal personality, and implements or withholds the treaty's commitments through its own legislation and courts. What flows to it: international standing as a treaty party, borders stabilized at Lausanne, and - where it limits implementation - discretionary leverage convertible in bilateral negotiations. What flows from it: the administrative acts on which the communities' institutional continuity actually depends. Exit would mean denouncing a settlement that fixed its territory and international recognition, at the cost of its European integration path and great-power relationships.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, generational, constrained, national).

% The signatory powers charged with supervising the minority clauses, Greece foremost among the active ones. Collect diplomatic leverage and kin-state protection credibility from the supervision role: raising Halki, vakf property, or Thrace mufti questions in bilateral and European fora is an instrument of influence as well as protection. Bound by a treaty role they cannot shed without abandoning co-ethnics and conceding the settlement's collapse; their enforcement effort has swung from active interwar supervision to decades of disengagement to renewed pressure through European mechanisms.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, lausanne_guarantor_states, beneficiary,
    institutional, generational, constrained, continental).

% The European Court of Human Rights, Council of Europe monitoring bodies, and the EU accession process now adjudicate and grade compliance with the minority guarantees. They take cases (the Prinkipo orphanage return), issue recommendations, and tie progress reports to accession chapters. They decide nothing about the treaty's meaning in advance; their rulings accumulate into the operative interpretation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_human_rights_mechanisms, observer,
    institutional, generational, analytical, continental).

% Syriac Orthodox, Chaldean, Protestant, Catholic, and Alevi communities live under the same state's general law with no treaty seat: the Lausanne definition enumerated only the Greek Orthodox, Armenian, and Jewish communities as non-Muslim minorities. They would object that the arrangement hard-codes a protected triad while their own schools, foundations, and training facilities depend on ordinary legislation and ad hoc permission. They are inside the affected territory and outside the conversation the treaty created.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, unrecognized_religious_communities, excluded,
    powerless, biographical, constrained, national).

% Greek islanders of Imvros/Gokceada, exempted from the 1923 population exchange, saw their schools closed, land registered away through discriminatory procedures, and their numbers fall from thousands to a few hundred. Their grievances sat outside the Istanbul-minority frame through which the protections were discussed for decades, reaching European courts only late. Those remaining are elderly and land-poor; exit was exercised by departure, and what is left cannot leave without losing the last of the patrimony.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, imvros_greek_islanders, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__expansive_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__expansive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes, by reciprocal treaty, the institutional terms of majority-minority coexistence after imperial collapse: which communities count as protected, who runs their schools, foundations, worship, and clergy formation, and what the host state must tolerate - settled once centrally instead of renegotiated in each crisis. The mirror provision (Muslims of Western Thrace) ties each state's compliance to the other's.
% TRANSFER_FUNCTION: Moves administrative authority over education, pious foundations, and religious leadership from the host state's discretionary control to the recognized communities' own institutions, and moves enforcement initiative to the guarantor powers and later European mechanisms. Security of institutional continuity flows to the minorities; supervision leverage flows to the guarantors.
% ABSENT_VOICES: Communities enumerated out of the treaty's minority definition - Syriac Orthodox, Chaldeans, Protestants, Catholics, Alevis - would object that the arrangement hard-codes a protected triad and leaves them to general law with no treaty seat. The Imvros islanders would object that their land and school grievances fell outside the Istanbul-minority frame for decades. Both sit inside the affected territory without a place in the conversation.
% DISAPPEARANCE_RATIONALE: If the protections vanished overnight, the surviving institutions lose their last international anchor: vakf litigation loses its treaty reference, the Thrace mirror unravels symmetrically, Greece loses its supervision mandate, and the remaining communities - already demographically decimated - face accelerated institutional dissolution. The administering state gains unrestricted discretion over the residue.
% FOUNDING_PROBLEM: After WWI and the Greco-Turkish war, the powers needed to close the Eastern Question without further partition: secure populations stranded by imperial collapse, stabilize the new borders, and make each state's treatment of the other's co-ethnics a matter of treaty consequence rather than domestic whim. Functional continuity of pre-existing religious governance was the chosen instrument because the millet-derived institutions were the communities' operative form.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the contracting states' own ratification instruments (Britain, France, Italy, Japan, Greece, Romania, the Serb-Croat-Slovene state, Turkey), by European Court of Human Rights case law treating the obligations as current (the Prinkipo orphanage judgment), by Council of Europe monitoring reports, and by the reciprocal invocation of the mirror provisions by the Western Thrace Muslim community. Stated plainly: the administering state does not corroborate the expansive content - it disputes the scope - so corroboration of this reading's substance rests on international mechanisms, the reciprocal seat, and independent historiography, not on the party whose compliance is at issue.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is rope: the arrangement's core is a genuine coordination function - fixing the institutional terms of coexistence by reciprocal treaty so neither administration renegotiates them per crisis - with the recognized communities as net beneficiaries and no group paying a transfer through the structure. The metrics are authored independently as descriptive facts. Extractiveness is low-moderate (0.32): the arrangement transfers authority TO the minorities rather than from them; its extraction consists of dependency and contingency exposure - institutional survival held to compliance cycles the communities do not control - which swelled during the mid-century enforcement collapse and partially receded with post-2008 restorations. Suppression (0.45, raw and unscaled) measures the external enforcement pressure currently required to hold the guarantee against administering-state drift: guarantor diplomacy, European Court rulings, accession conditionality. Theater ratio (0.48) is elevated honestly: reporting, commemoration, and progress-chapter activity run heavy relative to delivery (Halki remains closed more than fifty years on), but the functional core - schools running, foundations administered, worship continuing - is real, which is what separates this from inertial maintenance. Accessibility collapse (0.65): once the arrangement is understood, the communities' alternative set collapses toward inside-the-framework strategies, because exit means institutional dissolution; the administering state retains interpretive exits it has repeatedly exercised. Resistance (0.62): a century of interpretive resistance from the administering state - the 1936 Declaration, the 1974 expropriation rulings, the 1971 closure - punctuated by minority-side resort to European courts over guarantor channels. All three series run on one shared nine-point grid (1923-2024) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the four community seats the arrangement subsidizes: authority and continuity flow in, directionality sits near the beneficiary pole, and the lived experience is protective coordination - a lifeline whose failures are the administering state's breaches, not the arrangement's extraction. From the administering state's seat the same structure is a standing sovereignty reservation: it pays compliance costs and litigation exposure and gains stability and recognition, landing near-symmetric with a slight target tilt. From the guarantor seats it is an influence asset: supervision generates leverage whether or not enforcement delivers. The engine computes these divergent per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the four community seats and the guarantor states are declared beneficiaries, so their directionality sits near the subsidy pole - deepest for the identity-locked community seats, whose exit equals dissolution and who are therefore held inside the arrangement at any price, and moderated for the guarantor states, whose benefit is leverage they could partially replicate elsewhere. The administering state declares no beneficiary or victim position; it bears the arrangement's sovereignty and compliance costs while collecting stability and standing, placing it near-symmetric with a slight target tilt. No directionality override is authored: the override surface is keyed by power atom, and an institutional-atom override calibrated for the administering state would also drag the guarantor and European-mechanism seats off their derived positions; the near-symmetric placement is the honest structural read. The observer seat carries analytical directionality and feeds no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - stabilizing post-imperial coexistence by making minority treatment a treaty consequence - is still live: the communities persist, the mirror provision still disciplines both administrations, and the continuity question is unresolved. Status live paired with verdict world_rearranges yields no zombie flag, correctly: the arrangement is load-bearing for what remains. The mandatrophy risk sits elsewhere - in the enforcement-gap inertia the receipt surface records (diffuse gains, prohibitive fixing): the administering state could deliver most of the expansive frame by ordinary legislation at modest technical cost, but the political cost to it exceeds any benefit it perceives, so the gap persists without a capturer booking it. If the demographic basis of the Istanbul communities finishes collapsing, the frame's object thins toward commemoration and the arrangement would drift toward theatrical maintenance of a continuity fewer and fewer people inhabit - the theater-ratio series is the early-warning trace of exactly that drift. The rope claim keeps the live coordination core visible so that drift is measured against function, not assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_scope_line,
    'This constraint is one reading of the kernel lausanne_minority_protections: what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Interpretive adjudication - European Court clarification or negotiated settlement of the Articles 40-41 scope line; the sibling stories (restrictive_reading, guarantor_reading) carry the alternative structures.',
    'If the restrictive reading prevails, this constraint''s institutional content vanishes: beneficiaries reduce to individual worshippers, the dependency extraction disappears with the guarantee, and the arrangement collapses toward a bare worship floor. If the guarantor reading prevails, enforcement relocates externally and this reading''s suppression and extraction geometry shifts onto the supervision channel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_scope_line, conceptual, 'Committer structure: one of three readings of the Lausanne kernel; disagreement located in the Article 40-41 scope line.').

omega_variable(
    enforcement_locus_dependence,
    'Does the guarantee''s functionality depend on external enforcement (guarantor diplomacy, European mechanisms), or could domestic incorporation sustain it?',
    'Compare compliance episodes with and without external pressure: the post-2008 vakf restorations under accession conditionality versus the unpressured 1936-1974 erosion.',
    'If externally dependent, the arrangement is enforcement-dependent coordination whose suppression requirement tracks guarantor attention; if domestically sustainable, the mid-century collapse was contingent politics rather than structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_locus_dependence, empirical, 'Whether the protections'' operative force requires continuous external enforcement.').

omega_variable(
    dependency_cost_intrinsic_or_artifact,
    'Is the communities'' contingency exposure - institutional survival held to compliance cycles they do not control - an intrinsic cost of the guarantee structure or an artifact of enforcement decay?',
    'Counterfactual comparison with minority guarantees that include justiciable domestic incorporation elsewhere in Europe: if equivalent guarantees without the interstate channel carry lower contingency costs, the exposure is structural.',
    'If intrinsic, part of the measured extractiveness is the permanent price of the interstate-guarantee design and the rope reading must absorb it as coordination cost; if artifact, extractiveness falls toward the coordination floor as enforcement matures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_cost_intrinsic_or_artifact, conceptual, 'Whether the dependency extraction is structural to the guarantee design or a decay artifact.').

omega_variable(
    demographic_basis_sustainability,
    'Can the expansive frame''s object - functioning pre-1923 institutions - survive the demographic collapse of the communities that constitute them, or does continuity become commemorative?',
    'Track clergy ordinations, school enrollments, and foundation activity against community population; the Halki question is the leading indicator.',
    'If the constitutive populations finish collapsing, the theater ratio''s rise marks transition from functional guarantee to maintained memory, and the arrangement drifts toward inertial persistence with the text outliving its bearers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_basis_sustainability, empirical, 'Whether the guarantee''s object persists demographically or thins into commemoration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.12).
narrative_ontology:measurement(laus_tr_t1936, lausanne_minority_protections__expansive_reading, theater_ratio, 1936, 0.18).
narrative_ontology:measurement(laus_tr_t1955, lausanne_minority_protections__expansive_reading, theater_ratio, 1955, 0.3).
narrative_ontology:measurement(laus_tr_t1964, lausanne_minority_protections__expansive_reading, theater_ratio, 1964, 0.34).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__expansive_reading, theater_ratio, 1971, 0.4).
narrative_ontology:measurement(laus_tr_t1974, lausanne_minority_protections__expansive_reading, theater_ratio, 1974, 0.44).
narrative_ontology:measurement(laus_tr_t1992, lausanne_minority_protections__expansive_reading, theater_ratio, 1992, 0.56).
narrative_ontology:measurement(laus_tr_t2008, lausanne_minority_protections__expansive_reading, theater_ratio, 2008, 0.44).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__expansive_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.14).
narrative_ontology:measurement(laus_be_t1936, lausanne_minority_protections__expansive_reading, base_extractiveness, 1936, 0.21).
narrative_ontology:measurement(laus_be_t1955, lausanne_minority_protections__expansive_reading, base_extractiveness, 1955, 0.44).
narrative_ontology:measurement(laus_be_t1964, lausanne_minority_protections__expansive_reading, base_extractiveness, 1964, 0.5).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__expansive_reading, base_extractiveness, 1971, 0.53).
narrative_ontology:measurement(laus_be_t1974, lausanne_minority_protections__expansive_reading, base_extractiveness, 1974, 0.56).
narrative_ontology:measurement(laus_be_t1992, lausanne_minority_protections__expansive_reading, base_extractiveness, 1992, 0.51).
narrative_ontology:measurement(laus_be_t2008, lausanne_minority_protections__expansive_reading, base_extractiveness, 2008, 0.39).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__expansive_reading, base_extractiveness, 2024, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.58).
narrative_ontology:measurement(laus_su_t1936, lausanne_minority_protections__expansive_reading, suppression_requirement, 1936, 0.46).
narrative_ontology:measurement(laus_su_t1955, lausanne_minority_protections__expansive_reading, suppression_requirement, 1955, 0.16).
narrative_ontology:measurement(laus_su_t1964, lausanne_minority_protections__expansive_reading, suppression_requirement, 1964, 0.13).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__expansive_reading, suppression_requirement, 1971, 0.16).
narrative_ontology:measurement(laus_su_t1974, lausanne_minority_protections__expansive_reading, suppression_requirement, 1974, 0.19).
narrative_ontology:measurement(laus_su_t1992, lausanne_minority_protections__expansive_reading, suppression_requirement, 1992, 0.36).
narrative_ontology:measurement(laus_su_t2008, lausanne_minority_protections__expansive_reading, suppression_requirement, 2008, 0.57).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__expansive_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one treaty text (Articles 37-45, Treaty of Lausanne 1923), three structurally distinct claims. The restrictive reading authors a low-epsilon individual-worship floor with no institutional beneficiaries or victims; the guarantor reading authors an enforcement-locus claim whose extraction geometry turns on external supervision; this expansive reading authors a moderate-epsilon functional-continuity guarantee whose beneficiaries are the recognized communities and whose costs are dependency and contingency exposure. Upstream/downstream: the expansive scope claim supplies the content that the guarantor reading's enforcement machinery would deliver (expansive influences guarantor), while the restrictive reading is the administering state's implementation baseline that this reading's scope line directly contradicts. Each story carries its own epsilon, beneficiaries, and stakeholders; the family links record shared regulatory domain and causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
