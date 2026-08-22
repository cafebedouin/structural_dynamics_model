% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Human Heritage (Museum-Framing Reading)
 *   domain: cultural_heritage/religious_authority/sovereignty
 *
 * SUMMARY:
 *   This story instantiates the universal-heritage reading of the Hagia
 *   Sophia kernel: the claim that the site's legitimacy rests on its status
 *   as shared human cultural heritage transcending any single religious or
 *   national claim. Under this reading, legitimacy is administered by a
 *   technocratic secular apparatus (UNESCO's world-heritage machinery plus
 *   Turkish secularist museum administration) rather than by Islamic
 *   sovereignty (the sibling islamic_sovereignty_reading) or Orthodox
 *   ecclesiastical continuity (the sibling orthodox_restitution_reading). The
 *   1934 museum conversion is this reading's founding act; the 2020
 *   reconversion to mosque status is read here as an erosion event that the
 *   universal-heritage framing has structurally lost ground to. The
 *   extraction this story measures is the tourism/scholarship rent and
 *   secular-modernity ideological signal captured by treating the site as a
 *   depoliticized universal object, at the cost of suppressing the continuous
 *   Islamic worship claim of the local congregational community. This is a
 *   distinct constraint from the sibling readings — it has its own
 *   beneficiary set (global tourism, international scholarship, secularist
 *   elites, UNESCO apparatus), its own victim set (practicing Muslim
 *   worshippers, local religious community), and its own ε trajectory, which
 *   is why it is authored as a separate story per the ε-invariance principle
 *   rather than as one axis of a single multi-valued constraint.
 *
 * KEY AGENTS:
 *   - unesco_world_heritage_apparatus: agenda_setter/beneficiary (institutional/analytical) — certifies and defends the universal-value framing
 *   - secularist_turkish_elites: agenda_setter/beneficiary (institutional/constrained) — built and administer the museum-era legitimacy claim
 *   - global_tourism_sector: beneficiary (organized/mobile) — captures revenue from unrestricted universal access
 *   - international_heritage_scholarship: beneficiary (organized/mobile) — captures access and disciplinary authority
 *   - practicing_muslim_worshippers: payer (moderate/constrained) — bears suppressed worship access
 *   - local_religious_community_of_istanbul: payer/excluded (moderate/constrained) — bears the historic cost of museum-era restriction
 *   - orthodox_christian_diaspora: excluded (organized/trapped) — sees the universal frame as laundering Christian dispossession
 *   - turkish_national_government: agenda_setter (institutional/arbitrage) — sovereign authority able to switch framings entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.62).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Human Heritage (Museum-Framing Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/religious_authority/sovereignty").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '6be78eb9-f274-4c1d-b707-a35432510274').
narrative_ontology:cs_kernel_codification('6be78eb9-f274-4c1d-b707-a35432510274', distributed).
narrative_ontology:cs_authority_grounding('6be78eb9-f274-4c1d-b707-a35432510274', distributed).
narrative_ontology:cs_reading_relation('6be78eb9-f274-4c1d-b707-a35432510274', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('6be78eb9-f274-4c1d-b707-a35432510274', hagia_sophia_substrate__orthodox_restitution_reading, influences).
narrative_ontology:cs_axiom('6be78eb9-f274-4c1d-b707-a35432510274', foundational, cultural_significance_transcends_confessional_ownership).
narrative_ontology:cs_axiom_status(cultural_significance_transcends_confessional_ownership, holdable).
narrative_ontology:cs_axiom_grounding('6be78eb9-f274-4c1d-b707-a35432510274', cultural_significance_transcends_confessional_ownership, conventional).
narrative_ontology:cs_axiom('6be78eb9-f274-4c1d-b707-a35432510274', secondary, secular_technocratic_administration_is_neutral_arbiter).
narrative_ontology:cs_axiom_status(secular_technocratic_administration_is_neutral_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('6be78eb9-f274-4c1d-b707-a35432510274', secular_technocratic_administration_is_neutral_arbiter, instrumental).
narrative_ontology:cs_reference_frame('6be78eb9-f274-4c1d-b707-a35432510274', id_1934_kemalist_museum_secularization).
narrative_ontology:cs_drift_state('6be78eb9-f274-4c1d-b707-a35432510274', post_2020_reconversion_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('6be78eb9-f274-4c1d-b707-a35432510274', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_heritage_scholarship).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, unesco_world_heritage_apparatus).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, practicing_muslim_worshippers).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, local_religious_community_of_istanbul).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certifies the site as a World Heritage property under the 'outstanding universal value' framework, which requires treating it as a shared artifact of humanity rather than the property of any single faith or nation. This designation shapes funding, conservation mandates, and international pressure on the Turkish state, and the apparatus's own relevance depends on sites remaining legible as 'universal' rather than reverting to exclusive confessional use.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_world_heritage_apparatus, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, unesco_world_heritage_apparatus, beneficiary).

% Kemalist-descended bureaucratic, academic, and museum-administration elites who built and defended the 1934 museum conversion as a signature act of secular modernization. They administer the technocratic apparatus that historically kept the site out of active worship use, and their political and cultural legitimacy is bound up with the site standing as proof that Turkey is a secular, Western-facing state rather than a confessional one.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary).

% Tour operators, hospitality businesses, and the Turkish state tourism ministry derive substantial revenue from the site's status as a universally accessible monument that any visitor, regardless of faith, can enter as a spectator. Full conversion to active exclusive worship space threatens unrestricted visitor access, dress and behavior norms for tourists, and photography/viewing rights that the museum framing guarantees.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% Byzantinists, art historians, and conservation scientists depend on sustained physical and photographic access to mosaics, frescoes, and architectural features that active exclusive worship use has historically curtained, covered, or restricted during prayer times. Their disciplinary authority and funding streams are built on the site's continued legibility as a universal scholarly object rather than primarily a functioning mosque.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_heritage_scholarship, beneficiary,
    organized, generational, mobile, global).

% Ordinary worshippers who see the site as the historic Aya Sofya Mosque endowed by Sultan Mehmed II, and who experienced the museum era (1934-2020) as the suppression of an active, functioning waqf mosque in favor of a secular tourist attraction. Under the universal-heritage framing their claim to unrestricted daily prayer use is treated as one interest among many to be balanced against tourism and scholarship access, not as the primary legitimating fact of the site.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, practicing_muslim_worshippers, payer,
    moderate, biographical, constrained, national).

% Residents and congregational networks in the historic peninsula who regard the site's Islamic endowment as continuous since 1453 and who bore the practical cost of the museum era — restricted prayer access, tourist crowds during observance, mosaics veiled or unveiled according to visitor-management rather than liturgical logic. Their genealogical claim is treated by the universal-heritage frame as a parochial interest to be transcended rather than honored.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, local_religious_community_of_istanbul, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, local_religious_community_of_istanbul, excluded).

% Greek Orthodox and broader Christian communities who regard the site as the mother church of Eastern Christendom would, under a restitution reading, seek ecclesiastical access or symbolic primacy. The universal-heritage frame formally treats their claim as equivalent to any other confessional claim rather than as historically foundational, which functionally excludes their voice from privileged consideration even as it excludes the Islamic sovereignty claim too.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_diaspora, excluded,
    organized, civilizational, trapped, global).

% Holds sovereign authority over the site and has historically alternated between the museum framing (1934) and mosque reconversion (2020), using each shift to signal different domestic and international constituencies. Under the universal-heritage reading, the government's authority is exercised through the depoliticized machinery of technocratic heritage administration rather than through explicit religious or nationalist claims — though this framing is itself a strategic choice the state can and has reversed.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_national_government, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimating frame under which parties of different faiths and nationalities can access, study, and visit the site without any single group's exclusive claim triggering the others' exit or resistance — a genuine problem given the site's layered Byzantine-Ottoman history and its status as a global monument visited by millions annually regardless of their religious affiliation.
% TRANSFER_FUNCTION: Moves tourism revenue, scholarly access rights, and international prestige/legitimacy toward the global heritage sector and the secular Turkish administrative class, while moving unrestricted daily worship access and primary interpretive authority away from the local Muslim congregational community that historically used the site as an active mosque.
% ABSENT_VOICES: The Orthodox Christian diaspora, who would argue the universal-heritage frame launders a specifically Christian dispossession into a neutral-sounding cosmopolitanism; and ordinary Istanbul worshippers, who experience 'universal heritage' as a polite vocabulary for excluding them from a place their community endowed and used continuously for centuries. Neither is in the room when heritage-designation decisions are made.
% DISAPPEARANCE_RATIONALE: If the universal-heritage framing were abandoned, the site would almost certainly revert fully to the trajectory realized in 2020: full mosque status under Turkish state Islamic authority, restricted or reorganized visitor access during prayer, and the exit of the tourism/scholarship apparatus from its current privileged position. UNESCO's World Heritage designation and associated funding and diplomatic leverage would also be jeopardized, materially rearranging the site's international administration.
% FOUNDING_PROBLEM: In 1934, the site was converted from mosque to museum to resolve a genuine multi-claimant legitimacy problem: a monument with deep Byzantine Christian origins and five centuries of Ottoman Islamic use, situated in a new secular republic trying to signal a decisive break from both Ottoman religious governance and any claim of restoring it to Christendom.
% FOUNDING_PROBLEM_CORROBORATION: Secularist Turkish elites and UNESCO-affiliated heritage scholars attest the founding problem remains live — that any single-confession claim would reignite geopolitical and sectarian conflict. Turkish religious authorities, the 2020 Council of State ruling reconverting the site to mosque status, and independent legal historians outside the beneficiary set attest the founding problem was substantially a temporary Kemalist political expedient rather than a permanent resolution, and that the waqf endowment underlying Ottoman-era use was never legally extinguished — supporting a 'dead as universal solution, live as unresolved sovereignty question' reading.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is authored high because the universal-heritage frame channels substantial, concrete tourism revenue and scholarly access rights toward specific organized beneficiaries while the cost — suppressed unrestricted worship — falls on a specific, identifiable community whose historical claim predates the frame that now subordinates it. Suppression (0.62) reflects that enforcing the 'universal' status required active administrative and sometimes legal machinery (visitor rules, prayer-time restrictions, veiling of religious iconography during the museum era) rather than voluntary consensus. Theater ratio (0.48) is elevated because a substantial share of the framing's public performance — 'shared heritage of humanity' rhetoric — increasingly functions as legitimating cover for what is, on the ground, a contested and partially reversed administrative choice rather than a settled consensus; the 2020 reconversion is strong evidence the universal frame's grip was more theatrical than structurally secure. Accessibility collapse (0.5) and resistance (0.7) are both moderate-to-high: alternatives (mosque status, ecclesiastical status) never fully disappeared as live options — they remained visibly contested and one alternative (mosque status) has since been legally realized, which is why resistance is authored high rather than low.
 *
 * DIRECTIONALITY LOGIC:
 *   Global tourism, international scholarship, and secularist elites are declared beneficiaries because the universal-heritage frame is the mechanism by which their access, revenue, and ideological legitimacy are secured; their directionality sits near the beneficiary end and is amplified by their organized/institutional power and mobile/analytical exit options. Practicing Muslim worshippers and the local religious community are declared victims because the same frame is the mechanism suppressing their continuous worship claim; their constrained exit options (they cannot simply relocate their historic mosque) push their directionality toward the target end. The Orthodox diaspora is excluded rather than a clean victim or beneficiary — the universal frame formally treats their claim symmetrically with the Islamic claim, which from their perspective flattens a claim they consider historically primary into 'one interest among several,' a distinct grievance from active suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resolving a genuinely multi-claimant legitimacy dispute in a new secular republic — was real in 1934 and is authored here as contested rather than flatly dead: the underlying multi-claimant structure never went away, but the specific solution (permanent museum status enforced as the neutral default) has been legally and politically superseded by the 2020 reconversion. Authoring founding_problem_status as contested (rather than dead) prevents this story from either (a) treating the universal-heritage claim as pure ideological theater with no genuine underlying coordination problem, or (b) treating it as an eternally settled natural resolution immune to challenge. The mismatch between a status of 'contested' and a disappearance_verdict of 'world_rearranges' is itself diagnostic: the frame's own defenders treat the problem as unresolved and ongoing, which is consistent with a tangled_rope reading (real coordination function, real extraction) rather than either a pure snare or a stable rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_heritage_as_secular_cover_or_genuine_pluralism,
    'Is the universal-heritage framing a genuine solution to an intractable multi-claimant legitimacy problem, or is it a secular-nationalist ideological project that uses ''humanity''s shared heritage'' language to naturalize a specific 1934 political choice and disguise its beneficiary structure?',
    'Comparative analysis of how the ''universal heritage'' designation is invoked at other multi-claimant religious sites (e.g. the Temple Mount/Haram al-Sharif, the Church of the Nativity) to see whether the framing correlates systematically with secular-state administrative benefit, or whether it produces genuinely balanced access outcomes independent of which party controls the administering state.',
    'If the framing is shown to systematically track secular-elite and international-tourism benefit across cases rather than genuinely balanced access, this supports reclassifying the universal-heritage reading itself as a snare-adjacent extraction mechanism rather than tangled_rope; if it produces genuinely balanced outcomes elsewhere, the tangled_rope coordination function is better evidenced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_as_secular_cover_or_genuine_pluralism, conceptual, 'Whether universal-heritage framing is genuine multi-claimant coordination or secular-elite/tourism extraction dressed in cosmopolitan language.').

omega_variable(
    kernel_disagreement_locus_sovereignty_vs_universality,
    'Where exactly do the three kernel readings (islamic_sovereignty, orthodox_restitution, universal_heritage) actually disagree — is it about the underlying historical facts (waqf validity, Byzantine founding), or purely about which normative framework should govern a site whose facts are largely undisputed by all three readings?',
    'Documentary and legal analysis of the waqf endowment''s post-1934 legal status (never formally dissolved, per the 2020 Turkish Council of State ruling) versus the historical record of Byzantine ecclesiastical founding (undisputed) versus the normative claim that shared heritage should override either sovereignty claim.',
    'If the disagreement is purely normative (all sides agree on the facts, disagree on which framework should govern), this supports treating the three readings as coexisting rather than one foreclosing another. If a factual dispute (e.g., over waqf validity) is doing real work, that would sharpen which readings can coexist and which are in tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_locus_sovereignty_vs_universality, conceptual, 'Locating whether the kernel readings diverge on facts or purely on governing normative framework.').

omega_variable(
    reversibility_of_universal_frame_given_2020,
    'Given that the 2020 reconversion to mosque status already occurred, is the universal-heritage reading now describing a live administrative arrangement, a historical arrangement (1934-2020) whose extraction profile should be measured in the past tense, or an ongoing contested claim that international actors (UNESCO, foreign governments) continue to assert despite the change in on-the-ground status?',
    'Track whether UNESCO''s World Heritage Committee formally revises the site''s designation or statement of outstanding universal value in response to the 2020 change, versus continuing to assert the universal framing rhetorically without corresponding administrative authority.',
    'If UNESCO and international actors continue asserting the universal frame with no administrative teeth, the theater_ratio for this reading should trend higher still (pure rhetorical survival); if the framing is formally abandoned, this story''s interval should be understood as closed/historical rather than ongoing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reversibility_of_universal_frame_given_2020, empirical, 'Whether the universal-heritage reading remains administratively live post-2020 or has become primarily rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hagi_tr_t15, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(hagi_tr_t30, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(hagi_tr_t45, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(hagi_tr_t60, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 60, 0.43).
narrative_ontology:measurement(hagi_tr_t86, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 86, 0.46).
narrative_ontology:measurement(hagi_tr_t90, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 90, 0.48).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hagi_be_t15, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(hagi_be_t30, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(hagi_be_t45, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 45, 0.63).
narrative_ontology:measurement(hagi_be_t60, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(hagi_be_t86, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 86, 0.7).
narrative_ontology:measurement(hagi_be_t90, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hagi_su_t15, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(hagi_su_t30, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(hagi_su_t45, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(hagi_su_t60, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement(hagi_su_t86, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 86, 0.72).
narrative_ontology:measurement(hagi_su_t90, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 90, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__universal_heritage_reading, 0.1).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the hagia_sophia_substrate kernel. islamic_sovereignty_reading grounds legitimacy in the 1453 conquest and continuous waqf endowment under Turkish Islamic sovereignty; orthodox_restitution_reading grounds legitimacy in Byzantine Christian founding and seeks ecclesiastical restitution or neutrality; this story (universal_heritage_reading) grounds legitimacy in shared human heritage administered by secular/international technocratic authority. Each reading has a distinct beneficiary/victim structure and a distinct ε (this reading's ε=0.68, reflecting tourism/scholarship rent capture and suppression of worship access — not to be confused with or averaged against the siblings' ε values). The 2020 Turkish Council of State ruling reconverting the site to mosque status is the key structural event that shifted power decisively toward the islamic_sovereignty_reading and away from this reading, without fully extinguishing the universal-heritage claim internationally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
