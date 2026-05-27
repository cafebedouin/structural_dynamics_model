% ============================================================================
% CONSTRAINT STORY: absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_absolutist_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: absolutist_reading
 *   human_readable: Absolutist First Amendment Reading: 'No Law' Means No Law
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of the First Amendment instantiates a specific
 *   committer choice: that the constitutional text 'Congress shall make no
 *   law abridging freedom of speech' mandates categorical protection for all
 *   speech except narrow historical exclusions (seditious libel, fighting
 *   words, obscenity). This reading directly confronts two sibling readings
 *   that read the same text differently: the harm_limited_reading (permits
 *   restrictions on speech that causes direct, demonstrable harm to
 *   identifiable persons or groups) and the categorical_balancing_reading
 *   (permits categorical protection of core speech while requiring balancing
 *   for peripheral speech). The absolutist reading claims that the text
 *   itself is categorical and admits no balancing. However, the structural
 *   dynamics reveal that this reading benefits identifiable agents (speakers
 *   with platforms and capital, majority voices) while externalizing costs
 *   onto identifiable victims (targeted minorities facing coordinated
 *   harassment, incitement, and threats). The beneficiary-victim structure is
 *   asymmetric and enforced through judicial and institutional machinery:
 *   courts apply the reading as constitutional mandate, suppressing
 *   alternative framings. This makes the absolutist reading a Tangled Rope
 *   constraint: it combines a genuine coordination function (categorical
 *   rules simplify judicial line-drawing and protect speakers from arbitrary
 *   liability) with asymmetric extraction (denies remedies to harm victims,
 *   externalizes minority protection costs). The constraint's extractiveness
 *   has increased over the 40-year interval as digital platforms have enabled
 *   coordinated harassment and incitement at scale, making the externalized
 *   harms more visible and their costs more severe. The reading persists
 *   despite these costs through institutional inertia and the claim that it
 *   reflects textual meaning rather than a policy choice.
 *
 * KEY AGENTS:
 *   - Speakers and Majority Voices (powerful/arbitrage): Primary beneficiaries. Gain categorical protection from liability regardless of harm caused. Receive subsidy of speech costs through legal immunity.
 *   - Targeted Minorities and Vulnerable Populations (powerless/trapped): Primary victims. Bear full cost of speech harm (harassment, incitement, coordinated threats, defamation). Cannot exit the constitutional framework or organize effective alternative.
 *   - Judiciary (institutional/constrained): Institutional actor applying the reading as law. Benefits from categorical rule (reduces line-drawing burden) but constrained by obligation to deny remedies even for severe harms. Faces suppression through claim that reading is what Constitution mandates, not a policy choice.
 *   - Civil Society Institutions (moderate/constrained): Libraries, schools, community platforms. Coordinate through First Amendment protection but face extraction pressure when absolutist reading prevents them from excluding harm-causing speech. Constrained by constitutional doctrine even when institutional mission is threatened.
 *   - Democratic Coalition for Revision (organized/constrained): Voters, advocates, legislators perceiving absolutist reading as revisable. Face suppression through constitutional entrenchment but have exit path through amendment or court composition change.
 *   - Academic Constitutionalism (institutional/arbitrage): Originalist and textualist traditions maintain the reading through methodological authority. See the reading as degraded (requires increasingly strained historical claims to deny novel harms) but maintain it through institutional prestige rather than empirical justification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(absolutist_reading, 0.62).
domain_priors:suppression_score(absolutist_reading, 0.58).
domain_priors:theater_ratio(absolutist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(absolutist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(absolutist_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(absolutist_reading, tangled_rope).
narrative_ontology:human_readable(absolutist_reading, "Absolutist First Amendment Reading: 'No Law' Means No Law").
narrative_ontology:topic_domain(absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(absolutist_reading, '24789aac-e8bc-4ec1-a788-4778fa053a26').
narrative_ontology:cs_created_at('24789aac-e8bc-4ec1-a788-4778fa053a26', '').
narrative_ontology:cs_kernel_codification('24789aac-e8bc-4ec1-a788-4778fa053a26', fixed_text).
narrative_ontology:cs_authority_grounding('24789aac-e8bc-4ec1-a788-4778fa053a26', lineage).
narrative_ontology:cs_interpretation_layer_present('24789aac-e8bc-4ec1-a788-4778fa053a26').
narrative_ontology:cs_kernel_id(absolutist_reading, first_amendment_speech_protection).
narrative_ontology:cs_reading_relation('24789aac-e8bc-4ec1-a788-4778fa053a26', harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('24789aac-e8bc-4ec1-a788-4778fa053a26', categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('24789aac-e8bc-4ec1-a788-4778fa053a26', foundational, speech_protection_categorical_not_balancing).
narrative_ontology:cs_axiom_status(speech_protection_categorical_not_balancing, holdable).
narrative_ontology:cs_axiom_grounding('24789aac-e8bc-4ec1-a788-4778fa053a26', speech_protection_categorical_not_balancing, conventional).
narrative_ontology:cs_axiom('24789aac-e8bc-4ec1-a788-4778fa053a26', foundational, speaker_liability_incompatible_with_freedom).
narrative_ontology:cs_axiom_status(speaker_liability_incompatible_with_freedom, holdable).
narrative_ontology:cs_axiom_grounding('24789aac-e8bc-4ec1-a788-4778fa053a26', speaker_liability_incompatible_with_freedom, deontological).
narrative_ontology:cs_reference_frame('24789aac-e8bc-4ec1-a788-4778fa053a26', foundational_speaker_immunity).
narrative_ontology:cs_drift_state('24789aac-e8bc-4ec1-a788-4778fa053a26', contemporary_digital_scale, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(absolutist_reading, speakers_majoritarians).
narrative_ontology:constraint_beneficiary(absolutist_reading, powerful_voice_holders).
narrative_ontology:constraint_victim(absolutist_reading, targeted_minorities).
narrative_ontology:constraint_victim(absolutist_reading, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED MINORITY (SNARE) — Cannot exit the absolutist framework; bears full cost of speech harm (incitement, harassment, systematic defamation, coordinated deplatforming threats). The absolutist reading treats these harms as externalities acceptable for categorical speech protection. No organizational power, no alternative framework available within national constitutional authority. Maximum extraction experienced.
constraint_indexing:constraint_classification(absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY INSTITUTION (TANGLED ROPE) — Libraries, schools, community centers coordinate through First Amendment protection (coordination benefit) but are constrained by absolutist doctrine when facing pressure to host speech that harms their constituencies. Must enforce institutional boundaries while absolutist reading denies legitimate harm-based exclusion. Significant extraction: coordination benefits flow to speakers; harm-mitigation costs flow to institutions. Constrained exit — can lobby for carve-outs but cannot escape the national constitutional framework.
constraint_indexing:constraint_classification(absolutist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POWERFUL SPEAKER (ROPE) — Experiences absolutist reading as pure coordination: categorical protection enables speech without liability concern. Arbitrage exit (can publish, broadcast, organize without constraint). The absolutist framework subsidizes their speech costs and protects them from reputational or legal consequence. Experiences the constraint as coordination, not extraction — it solves their collective action problem (speaking without fear of suit).
constraint_indexing:constraint_classification(absolutist_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY APPLYING ABSOLUTIST READING (TANGLED ROPE) — Coordinates through the rule of law (categorical rule: 'no law' means no law reduces line-drawing costs). But also faces extraction pressure: must deny remedies to harm victims even when harm is severe and coordinated. Constrained by the constitutional text itself — the reading is claimed to be what the text says, not a policy choice. Significant suppression: courts cannot revise the reading without overturning precedent or amending the Constitution.
constraint_indexing:constraint_classification(absolutist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC COALITION SEEKING REVISION (SCAFFOLD) — Organized agents (voting constituencies, advocacy groups, legislators) see absolutist reading as a temporary constitutional arrangement that can be revised through amendment or democratic pressure on courts. Lower effective extraction because the coalition has exit paths: 27th Amendment process, Supreme Court composition change, institutional evolution of interpretation. Theater is moderate — the absolutist reading claims to be objective constitutional meaning, but the coalition perceives it as a political choice vulnerable to democratic contestation.
constraint_indexing:constraint_classification(absolutist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTELLECTUAL TRADITION / ACADEMIC CONSTITUTIONALISM (PITON) — The absolutist reading persists in jurisprudence and constitutional theory partly through institutional inertia. Originalist methodology claims to ground the reading in historical meaning ('no law' is categorical), but the actual functional role of the reading is to protect speakers from liability — a contemporary policy goal, not a historical discovery. Theater ratio is high: the academic apparatus of textual proof-texting and originalist rigor maintains the reading's legitimacy even as its practical function (shielding powerful speakers from responsibility) becomes more visible. The tradition sees its own reasoning as degraded when scrutinized through harm-analysis lenses, but maintains the reading through methodological authority rather than empirical justification.
constraint_indexing:constraint_classification(absolutist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TEXTUAL NATURALISM (MOUNTAIN) — From a civilizational perspective, the absolutist reading appears as an immutable property of the constitutional text itself: 'Congress shall make no law' is linguistically categorical, admits no exceptions, and therefore constrains all subsequent interpretation as a matter of logical necessity. This perspective sees the reading as not a choice but a discoverable fact about what the text means. However, structural data will trigger false summit detection: identifiable beneficiaries (speakers, powerful voices) exist, and the constraint's extraction mechanism (denying remedies to harm victims) is a contingent institutional arrangement grounded in a particular interpretive tradition, not a law of textual nature.
constraint_indexing:constraint_classification(absolutist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(absolutist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(absolutist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(absolutist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(absolutist_reading, TR),
    TR >= 0.70.

:- end_tests(absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The reading provides genuine coordination benefit (categorical rule simplifies adjudication, reduces speaker liability risk), but the asymmetry is severe: the coordination benefit flows primarily to speakers with existing platforms (powerful voices), while the extraction (denial of remedies) is concentrated on targeted minorities with no platforms. The 40-year measurement trajectory shows increasing extractiveness as digital coordination has enabled speech harms to scale faster than legal remedies can be developed. The reading's claim to be textually mandated adds suppression by blocking alternative framings at the constitutional level. Suppression (0.58): Moderate-high. Significant barriers to exiting or revising the reading: textual entrenchment (the reading claims to be what 'no law' means), judicial precedent binding lower courts, originalist methodology treating historical exceptions as the only permissible carve-outs. Alternative readings (harm_limited, categorical_balancing) exist but are suppressed by the claim that absolutist reading is what the Constitution requires. Victims cannot organize alternative constitutional framework without amendment (near-impossible). Theater ratio (0.35): Moderate-low. The absolutist reading claims to be derived from textual meaning, and this claim is maintained through academic and judicial apparatus (originalist proof-texting, textual analysis). However, the theater is lower than piton-level because the coordination function is genuine (categorical rule does reduce adjudication burden) and the reading's empirical track record is still partially defensible (most speech does not cause severe coordinated harm, so the cost of categorical protection is often null in practice). Theater would increase if contemporary harms (algorithmic incitement, coordinated harassment) forced courts into increasingly strained interpretations of 'narrow historical exclusions' to deny remedies.
 *
 * PERSPECTIVAL GAP:
 *   The absolutist reading produces maximal perspectival divergence despite unified textual claim. Speakers perceive pure coordination (Rope) — the reading solves their collective action problem (speech without liability). The judiciary perceives hybrid coordination-extraction (Tangled Rope) — categorical rule coordinates adjudication but suppresses harm remedies. Targeted minorities perceive pure extraction (Snare) — categorical protection for others' speech, zero remedies for their harm, no exit. Civil society perceives extraction from both sides (Tangled Rope, opposite vector) — must coordinate speech access while denying institutional boundaries. Democratic coalition perceives temporary constraint with revision path (Scaffold) — the reading can be changed through amendment or court evolution. Academic tradition perceives degraded institution (Piton) — originalist methodology maintains the reading through historical claims that become increasingly strained as novel harms emerge. Analytical observer risks perceiving natural law (Mountain) — 'no law' seems categorically textual and immutable. This gap-set reveals that the 'unified reading' is actually four different constraints experienced by different agents: a coordination mechanism for speakers, an extraction apparatus for minorities, a methodological tradition for academics, and a revisable political choice for democrats.
 *
 * DIRECTIONALITY LOGIC:
 *   The absolutist reading's directionality is determined by the structural flow of benefits and costs. Speakers (powerful/arbitrage) experience low directionality (d ≈ 0.15): they are net beneficiaries, facing categorical legal protection with no corresponding liability. The sigmoid f(d) produces negative effective extraction for this group — the constraint subsidizes their speech. Targeted minorities (powerless/trapped) experience high directionality (d ≈ 0.95): they are net targets, bearing legal helplessness in the face of speech harm, with no remedy and no exit. The sigmoid produces maximum experienced extraction (f(d) ≈ 1.42). Judiciary (institutional/constrained) experiences moderate-high directionality (d ≈ 0.60): they gain coordination benefit from categorical rule (reducing d below pure victim status) but are constrained by obligation to deny remedies. Civil society (moderate/constrained) experiences moderate directionality (d ≈ 0.55): mixed benefit from speech coordination and extraction from inability to exclude harmful speech. The perspectival gaps reflect these directionality differences: speakers see rope, judiciary sees tangled_rope, minorities see snare, civil society sees tangled_rope from opposite direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading resolves the mandatrophy by declaring that the text itself mandates the reading — it is not a policy choice but a linguistic fact about what 'no law' means. This claim is the constraint's central assertion: the reading is constitutional, not political. However, the structural data reveals mandatrophy: (1) identifiable beneficiaries (speakers, majority voices) exist and benefit systematically; (2) identifiable victims (targeted minorities) exist and bear systematic harms; (3) alternative readings (harm_limited, categorical_balancing) can be derived from the same text; (4) the choice to instantiate absolutist reading requires accepting that minorities' harm is an externality, not a constitutional concern. The mandatrophy resolution is: the reading is not mandated by the text (the text admits multiple readings), but instantiated by a committer commitment that categorical speaker protection is the highest good even at cost to vulnerable populations. The constraint is therefore Tangled Rope (hybrid coordination-extraction) grounded in a specific normative choice, not Mountain (natural law of meaning). False summit: the analytical observer's mountain classification is the committer's attempt to naturalize a political choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_carveout_definiteness,
    'What counts as a ''narrow historical exclusion'' permitted by the absolutist reading, and who decides when new harms should be categorized as historical vs novel?',
    'Historical analysis: what did Founders exclude (seditious libel, fighting words, obscenity)? Do contemporary harms (coordinated harassment, deepfakes, algorithmic incitement) fit historical categories or require new exclusions? If new exclusions are needed, does the reading remain ''categorical'' or become harm-balancing?',
    'If historical exclusions can be reinterpreted to cover contemporary harms: reading degrades to categorical_balancing_reading. If historical exclusions are fixed and inexpandable: reading becomes more clearly extractive (denies remedies for new harms). If ambiguous: the constraint''s suppression increases as courts face impossible line-drawing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_carveout_definiteness, empirical, 'Whether ''narrow historical exclusions'' can accommodate contemporary harms or require reading revision').

omega_variable(
    speaker_identity_asymmetry,
    'Does the absolutist reading protect speakers equally, or does its categorical protection systematically benefit speakers with existing platforms, capital, and access (majority speakers) while leaving vulnerable speakers unprotected (targeted minorities have less speech capacity when threatened)?',
    'Empirical speech distribution analysis: Do harm-targeted minorities actually speak more freely under absolutist protection, or does the removal of legal remedies against coordinated harassment/incitement reduce their effective speech capacity? Comparative analysis: does categorical protection correlate with increased minority voice or with greater majority dominance of public discourse?',
    'If asymmetry confirmed: absolutist reading externalizes harm to vulnerable speakers and benefits majority speakers. If symmetric: reading is genuinely protective rather than extractive. If conditional (depends on platform structure, media concentration): constraint''s extractiveness is network-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speaker_identity_asymmetry, empirical, 'Whether absolutist protection benefits speakers equally or systematically favors majority speakers').

omega_variable(
    reading_versus_historical_fact,
    'Is the absolutist reading a discovery of what the historical text actually meant to the Founders, or a choice among multiple defensible readings of the same text?',
    'Historical scholarship: what did the Founders say about the scope of ''no law''? Did they explicitly intend categorical protection or acknowledge exceptions (seditious libel, incitement, fraud)? Are originalist claims about historical meaning supported by primary sources or imposed retrospectively? Cross-reading comparative analysis: can the text support harm_limited_reading and categorical_balancing_reading with equal historical plausibility?',
    'If reading is historical fact: constraint is mountain (natural law of constitutional meaning). If reading is choice among defensible options: constraint is tangled_rope or snare (contingent institutional arrangement). If reading is recontextualized (Founders meant something else; reading imposed later): false summit confirmed, piton classification accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_historical_fact, conceptual, 'Whether absolutist reading is historical fact or interpretive choice').

omega_variable(
    remedy_externality_cost_distribution,
    'Who bears the actual cost of denying legal remedies for speech harm — is it genuinely borne by harm victims, or is the cost socialized through other institutions (police, prisons, hospitals, social services addressing downstream effects)?',
    'Cost accounting: track harm externalities that flow through public services (hate crime investigation, harassment counseling, threat assessment, campus security) because legal liability is blocked by absolutist reading. Compare: do jurisdictions with more restrictive speech liability frameworks have lower total social costs, or do they shift costs rather than eliminate them?',
    'If cost is genuinely borne by individuals: extractiveness is concentrated (high chi for victims). If cost is socialized: extractiveness is diffused (moderate chi; public bears cost not recorded as speech-related). If cost is redistributive (wealthy speakers benefit, less wealthy pay social taxes): reading exacerbates inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_externality_cost_distribution, empirical, 'How speech-harm costs are distributed: concentrated on victims, socialized, or redistributive').

omega_variable(
    reading_as_committer_choice,
    'To what degree is the absolutist reading grounded in the First Amendment text itself, versus grounded in the contemporary committer commitment that ''freedom from liability is the highest constitutional good even at the cost of harm to minorities''?',
    'Normative comparative framing: Can the same constitutional text be read as protecting core speech while permitting harm remedies (harm_limited_reading)? Can it be read as requiring categorical protection only for speech not coordinated into incitement/harassment (categorical_balancing_reading)? If the text admits multiple readings, the constraint''s classification reflects the committer''s choice of which reading to instantiate, not a textual mandate. The choice itself (prioritizing speaker immunity over remedy) is the structural commitment.',
    'If reading is committer-dependent: the constraint is Tangled Rope or Snare (hybrid/extractive institution) grounded in a specific normative commitment. If reading is text-determined: the constraint is mountain (natural law of meaning). The omega resolves the false summit question: is this a natural law or a contingent institutional reading?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_as_committer_choice, conceptual, 'Whether absolutist reading is mandated by the text or represents a committer''s normative choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(absolutist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abso_tr_t0, absolutist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(abso_tr_t20, absolutist_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(abso_tr_t40, absolutist_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(abso_be_t0, absolutist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(abso_be_t20, absolutist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(abso_be_t40, absolutist_reading, base_extractiveness, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(absolutist_reading, categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% The absolutist reading, harm_limited reading, and categorical_balancing reading form a kernel family: three structurally distinct constraints derived from the same constitutional text but with different ε values and beneficiary-victim structures. The absolutist reading (this file) has ε ≈ 0.62 (Tangled Rope) because it coordinates judicial line-drawing while extracting through denial of remedies. The harm_limited reading would have ε ≈ 0.35 (Rope) because it prioritizes victim protection with lower extraction. The categorical_balancing reading would have ε ≈ 0.45 (Tangled Rope, different vector) because it protects core speech while permitting peripheral speech balancing. All three are readings of the same kernel; none can be said to be 'correct' without specifying committer commitments. The network edges show influence: the absolutist reading's prioritization of categorical protection influences (constrains) both sibling readings by forcing them to defend their narrowing of the protected set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
