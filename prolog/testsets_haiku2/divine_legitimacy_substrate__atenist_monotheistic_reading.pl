% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Divine Legitimacy (Pharaonic Monopoly)
 *   domain: religious/political/economic
 *
 * SUMMARY:
 *   Pharaoh Akhenaten (ruled ~1353–1336 BCE) declared Aten the sole
 *   legitimate deity and proclaimed exclusive pharaonic revelation of Aten's
 *   truth, invalidating all other gods. This constraint story instantiates
 *   the Atenist monotheistic reading of the contested kernel 'divine
 *   legitimacy substrate.' The reading asserts that legitimate religious
 *   authority flows ONLY through the pharaoh's direct revelation of Aten; all
 *   other deities are false idols and their priesthoods are illegitimate
 *   authorities peddling superstition. This reading is one of three contested
 *   framings: the Amun polytheistic reading (traditional priestly authority
 *   through multi-deity cosmology) and the folk syncretistic reading
 *   (household/village pragmatic pluralism) are siblings in the same kernel.
 *   This story generates the Atenist reading as a clean, ε-invariant
 *   constraint: high extractiveness (0.82), high suppression (0.88), moderate
 *   theater (0.42), high accessibility collapse (0.79) because the claim of
 *   exclusive revelation logically precludes alternative deities from being
 *   true, and substantial resistance (0.71) from displaced priesthoods and
 *   folk practitioners.
 *
 * KEY AGENTS:
 *   - Pharaonic office: institutional agenda-setter, controls Aten revelation monopoly, redistributes temple wealth to state and Aten priesthood
 *   - Aten priesthood: appointed beneficiary, dependent on pharaonic patronage, operates new cult infrastructure
 *   - Traditional priestly class: powerful targets, lose official status, endowments, political influence, forced into accommodation or exile
 *   - Temple economies: organized payers, multi-generational economic institutions dismantled or repurposed, communities lose primary support
 *   - Household folk practitioners: powerless payers, identity-locked (religious identity constituted through practice), criminalized for continuing family ritual
 *   - Competing priestly networks: excluded institutional actors, structurally barred from legitimate authority
 *   - Royal administration: institutional beneficiary, gains consolidated power over religious and economic apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.82).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.88).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Divine Legitimacy (Pharaonic Monopoly)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political/economic").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '9959fa22-3563-49b1-b1ec-1d16e7f43bfd').
narrative_ontology:cs_kernel_codification('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', formalized).
narrative_ontology:cs_authority_grounding('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', extraction).
narrative_ontology:cs_interpretation_layer_present('9959fa22-3563-49b1-b1ec-1d16e7f43bfd').
narrative_ontology:cs_reading_relation('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', foundational, aten_sole_truth).
narrative_ontology:cs_axiom_status(aten_sole_truth, holdable).
narrative_ontology:cs_axiom_grounding('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', aten_sole_truth, empirically_contingent).
narrative_ontology:cs_axiom('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', foundational, pharaonic_exclusive_revelation).
narrative_ontology:cs_axiom_status(pharaonic_exclusive_revelation, holdable).
narrative_ontology:cs_axiom_grounding('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', pharaonic_exclusive_revelation, deontological).
narrative_ontology:cs_reference_frame('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', aten_monotheistic_authority).
narrative_ontology:cs_drift_state('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', post_akhenaten_succession, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('9959fa22-3563-49b1-b1ec-1d16e7f43bfd', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_office).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_economies).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_priestly_class).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, household_folk_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the agenda for all religious practice, declares Aten the sole true deity, establishes itself as Aten's exclusive earthly revelation channel, controls all theological adjudication. Defunds traditional temples, redirects their property and wealth to Aten priesthood and state coffers. The pharaoh claims direct revelation from Aten and styles himself as the bridge between the divine and human realms. Maintains the constraint through controlling temple hierarchies, appointing Aten priesthood, and enforcing theological conformity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_office, agenda_setter,
    institutional, generational, arbitrage, national).

% New religious establishment appointed by and dependent on the pharaoh. Operates temples and rituals dedicated to Aten, controls the theological curriculum, receives state funding and confiscated property from displaced temple economies. Their authority derives wholly from pharaonic appointment; they have no independent power base or lineage but gain substantial institutional resources and prestige. They benefit from the constraint's enforcement against their competitors (traditional priesthoods).
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_priesthood, beneficiary,
    organized, biographical, mobile, national).

% Priests of Amun, Ptah, Sekhmet, Khonsu, and other deities lose official status, temple endowments, political authority, and the right to adjudicate religious truth. Their priestly titles are delegitimized; continuing to serve their deities becomes heresy. Some accommodate by adopting Aten theology and joining the new priesthood; many are demoted, expelled, or forced into subsistence roles. Their accumulated wealth, institutional legitimacy, and generational authority are transferred to the state and Aten establishment. This is the most organized and resource-rich payer group.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_priestly_class, payer,
    powerful, generational, constrained, national).

% Multi-generational economic institutions—temple estates controlling vast lands, serfs, grain stores, craft workshops, trade networks—are dismantled, repurposed, or absorbed into state administration. These were the primary employers outside agriculture, the repositories of technical knowledge (medicine, mathematics, architecture), and the distributors of famine relief. Communities dependent on temple patronage for employment, apprenticeship, and grain distribution lose their primary institution. The economic disruption cascades through dependent populations.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_economies, payer,
    organized, generational, trapped, national).

% Rural and urban households maintain household shrines and family rituals honoring protective and childbirth deities (Taweret, Bes, Heka, local tutelary gods) for daily survival, fertility, and household security. These practices are now declared false superstition and heresy. Continuing them requires hiding shrines, risking punishment, and severing public religious identity. The religious identity—'one who honors the household gods'—is foundational to family cohesion and intergenerational meaning transmission. Exit means accepting that ancestral practice was superstition and rejecting the cosmology that structured family identity. Most cannot or will not do so; they practice in hiding or internal exile.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, household_folk_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Autonomous priesthoods—particularly Amun's Theban establishment, which possessed independent wealth, political alliances, and the authority to adjudicate theological questions—are structurally barred from legitimate religious authority. They are not negotiated with; they are expropriated. Their temples are defunded or repurposed, their priests are demoted or exiled, their claim to speak for the deities is declared false. If permitted to exist, they would compete directly with the pharaonic monopoly for religious authority and resources. Their exclusion is the enforcement object itself.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, competing_priestly_networks, excluded,
    powerful, generational, trapped, regional).

% The bureaucratic and military apparatus gains consolidated control over a massive economic and legitimacy apparatus. Temple properties flow to the crown, eliminating independent institutional power centers and competitors to royal authority. The constraint enables centralization of resources for military campaigns, monumental construction, and bureaucratic expansion. Removes a traditional check on pharaonic power: priesthoods could previously question or resist royal decisions on theological grounds, now impossible.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_administration, beneficiary,
    institutional, generational, analytical, national).

% Later historians, Egyptologists, and comparative religionists observe the Atenist period and debate its nature. They examine temple records, administrative documents, artistic evidence, and the rapid reversal of Atenism after Akhenaten's death to assess whether the constraint represented genuine theological innovation or political consolidation disguised as theology. They see the material evidence of wealth redistribution, priestly displacement, and suppression but dispute the ultimate framing.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, foreign_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_office).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies Egyptian religious authority under a single interpretive framework, eliminating competing priestly claims to divine truth and institutional autonomy. Replaces a loosely coordinated polytheistic system—where multiple deities and priesthoods coexist with overlapping jurisdictions, each claiming access to divine truth through their tradition—with a centralized monotheistic authority where all theological questions channel through the pharaonic office. Solves the coordination problem of 'how do we resolve conflicting divine claims?' by declaring all claims except the pharaoh's false.
% TRANSFER_FUNCTION: Transfers religious authority, institutional wealth, political influence, and legitimacy from independent priesthoods and household practice to the pharaonic office and its appointed Aten priesthood. Moves economic resources (agricultural land, serfs, trade networks, craft production, grain stores) from multi-generational temple endowments to state administration and Aten cult infrastructure. Suppresses private religious practice (household shrines, folk ritual, family theology) in favor of state-mandated Aten theology. Pharaonic office receives exclusive claim to divine revelation; Aten priesthood receives institutional funding and property; traditional priesthoods lose status, wealth, and authority; temple economies are dismantled; folk practitioners lose the right to public religious identity.
% ABSENT_VOICES: Traditional priesthoods are excluded from legitimate theological discourse—their thousands of years of accumulated wisdom are declared false. Household practitioners are silenced—their religious practice is criminalized, forcing them into hiding or apostasy. Regional priestly networks, especially Amun's Theban establishment, have no seat at the table of authority. Foreign merchants and priests whose own deities were honored in Egyptian temples are barred from legitimate worship. If these voices were present, they would argue strenuously that their deities are true, their priesthoods are legitimate, their practices are essential to Egypt's prosperity and order. They would deny the founding problem (that competing priesthoods destabilize pharaonic authority) and insist that plurality and autonomy are possible. But the constraint structures them out of the conversation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight—if the pharaonic claim to exclusive Aten revelation were withdrawn, traditional deities were returned to legitimate status, and household religious practice was decriminalized—the world would reorganize rapidly. Temple economies would be reconstituted (survivors would rebuild); traditional priesthoods would recover their authority and property (or descendants would reclaim them); household shrines would reappear openly in every home; the priestly hierarchies would resume independent adjudication of theological questions. The entire political economy of Egypt would shift from pharaonic theological monopoly back toward institutional pluralism. The constraint is not a feature of the natural world—its disappearance would be consequential.
% FOUNDING_PROBLEM: Egypt's pharaonic authority was partially constrained by autonomous priesthoods, especially Amun's Theban establishment, whose independent wealth and theological authority enabled them to resist or negotiate royal directives on military, economic, and political matters. Priestly coalitions could marshal the population against unpopular policies by invoking divine will. The pharaonic office lacked sufficient power concentration to impose its vision against organized priestly resistance.
% FOUNDING_PROBLEM_CORROBORATION: The pharaonic office and Aten priesthood attest the founding problem is the chaos of false idolatry and the spiritual degradation caused by worshiping non-existent deities—they present Aten monotheism as liberation from superstition and error. However, Egyptologists examining administrative documents, temple records, and the period immediately before Atenism find evidence of a very different founding problem: Amun's priesthood had accumulated land holdings rivaling the crown, had independent military alliances, and had successfully resisted pharaonic conscription. By the time of Akhenaten's reign, the traditional priestly class had already been substantially domesticated or accommodated. The founding problem—priestly autonomy constraining pharaonic power—was substantially solved or in decline before Atenism arrived. This suggests the constraint persists not to solve the original problem (which is gone) but because the pharaonic office has become dependent on the Aten theology for its own legitimacy.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.58 to 0.82 across the interval as the pharaonic office consolidates control: initial ambiguity about Aten theology gives way to active property confiscation and priestly displacement. Suppression rises similarly (0.65→0.88) because enforcement machinery must expand to prevent backsliding to traditional practice, exile displaced priesthoods, and police household shrines. Theater ratio rises from 0.18 to 0.42 because as time progresses, the theological legitimacy claims become less credible to those paying (traditional priesthoods know their gods were functional; households know their household gods protected them) while enforcement becomes more theatrical—punishing practice and controlling narrative rather than solving the stated coordination problem. Accessibility collapse is high (0.79) because the claim of exclusive revelation creates a logical closure: if Aten is the only true god and pharaonic revelation is the only legitimate source, alternatives are by definition inaccessible truths. Resistance is substantial (0.71) because the displaced priesthoods retain institutional memory and resource bases, household practitioners resist silently through hidden shrines, and the folk cosmology is deeply embedded in community identity.
 *
 * PERSPECTIVAL GAP:
 *   The pharaonic office and Aten priesthood seats would compute this constraint as high-coordination (solving competing priesthood chaos, unifying a fractured religious authority). The displaced priesthoods, temple communities, and folk practitioners compute it as pure extraction—they had legitimate authority and were expropriated; the coordination framing is post-hoc rationalization. The engine's per-seat computation should show a sharp divergence: from the pharaonic seat, this is tangled rope (genuine coordination + asymmetric beneficiary position); from the payer seats, this reads as snare (the coordination function is minimal, the extraction and suppression are overwhelming). The authored claim is tangled_rope (reflecting the constraint's structure: real coordination AND real extraction), which the divergent seat computations should vindicate.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaonic office and Aten priesthood are beneficiaries with high directionality toward 0.0 (they collect authority and wealth); traditional priesthoods and temple economies are high-directionality targets (~1.0) because they bear extraction through expropriation and lose institutional autonomy; household folk practitioners are identity-locked high-directionality targets (~0.95) because exit means severing religious identity constituted through practice and accepting theological nullification. The royal administration sits near beneficiary (~0.1) because it consolidates power without directly collecting religious authority—it gains the political spoils of centralization. Competing priestly networks are excluded structural targets (~0.95) because they are barred from legitimacy by the same structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—that competing priesthoods constrain pharaonic power—is solved. The constraint persists not because the problem is live (the priesthoods are eliminated or subservient) but because the pharaonic office has become dependent on the claim of exclusive Aten revelation for its legitimacy. Declaring the problem dead (priesthoods are no longer autonomous) would require admitting the theology was instrumental (a tool for consolidation) rather than truth. The constraint exhibits classic mandatrophy: the founding mandate has died but the structure persists through reframing (declaring household religion false to maintain suppression, even after priesthood threat is gone). The high theater_ratio trajectory (0.18→0.42) is the symptom: as the founding problem becomes increasingly dead, more enforcement activity goes to theatrical maintenance (punishing private devotion, teaching correct theology) rather than solving the original problem (neutralizing priesthood threat, which is already solved).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_claim_vs_political_consolidation,
    'Is Atenism a genuine theological innovation (Akhenaten''s sincere belief in Aten''s exclusive truth), or is it instrumentally designed to consolidate pharaonic power by eliminating autonomous priesthoods?',
    'Examine Akhenaten''s personal writings, artistic depictions, and ritual practices for evidence of theological consistency independent of political benefit. Compare the timing of theological claims with priestly expropriation and military/administrative centralization. Post-reign analysis: does the constraint persist and intensify after the founding problem (priestly independence) is solved?',
    'If theological: the extraction is a secondary effect of genuine belief, and the constraint reads as tangled rope (real coordination + incidental asymmetry). If instrumental: the constraint reads as snare (extraction is primary, theological framing is cover story). The omegas in mandatrophy_analysis suggest the instrumental reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_claim_vs_political_consolidation, conceptual, 'Whether Atenism is sincere theological innovation or political consolidation disguised as theology.').

omega_variable(
    monotheism_as_natural_law_claim,
    'Is the Atenist claim that ''Aten alone is true and all other deities are false'' asserted as natural law (an objective fact about divinity), or as a normative commitment (a choice to privilege one deity)?',
    'Examine theological texts for language of discovery (this is how things ARE) vs. language of obligation (this is how things SHOULD be). Compare claims to treatment of Egyptian cosmology before Atenism—did earlier Egyptians have different but equally valid readings, or is Atenism asserting a breakthrough to exclusive truth? Survey successor regimes'' theological stances: did they reinstate other deities as equally true, or reinstate them as secondary?',
    'If natural law: the accessibility collapse and resistance profiles are explained by the logical closure of exclusive truth, and the constraint reads as more Mountain-like (rigid, nearly unavoidable). If normative commitment: the high resistance and theater ratio are explained by active maintenance of a chosen framework against alternatives, and the constraint reads as more Snare-like (extraction riding on a contingent choice). This is the false-summit ambiguity: a false natural law claim (monotheism as discovered truth) benefits the pharaonic office as much as a deliberate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monotheism_as_natural_law_claim, conceptual, 'Whether exclusive monotheism is natural-law fact or normative commitment.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.88) structural (enforced through punishment, property confiscation, policing of practice), internalized (target populations believe the Atenist theology and suppress themselves), or both, and in what proportion?',
    'Post-suppression evidence: after the constraint ends (Tutankhamun reverses Atenism), how quickly do traditional priesthoods and household gods reappear? Rapid reappearance suggests suppression was structural; delayed or weak reappearance suggests internalized acceptance. Interview/narrative evidence: do displaced priesthoods and folk practitioners describe belief or coercion as the primary barrier?',
    'If purely structural: the constraint''s power depends on continuous enforcement machinery; removal of enforcement allows rapid coordination around alternative deities. If substantially internalized: the constraint''s effects persist even after formal reversal; the suppressed populations carry the suppression narratively and institutionally. Mixed suppression means the constraint''s effective hold is higher than the structural enforcement profile suggests, and fixing it is costlier.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    reading_family_sibling_status,
    'Are the Atenist, Amun polytheistic, and folk syncretistic readings genuine siblings (different contestable framings of the same kernel), or are some of them pre-kernel alternatives that the Atenist reading supersedes?',
    'Examine whether folk household practice and Amun priesthood authority existed simultaneously as live options before Atenism declared them false. If they coexisted as competing legitimate authorities, they are siblings. If folk practice was marginal/unformalized and Amun was unchallenged, then Atenism creates the reading contest ex nihilo (not a sibling relationship, but a reading that refabricates alternatives as siblings retroactively).',
    'If genuine siblings: the constraint''s suppression is directed against live competitor framings, and the constraint reads as snare (extraction from competitors). If Atenism refabricates the contest: the suppression is directed against practices that weren''t coded as readings until Atenism declared them false, which reframes the constraint as creative of its own opposition—a different extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_family_sibling_status, conceptual, 'Whether the three readings are genuine siblings or whether Atenism creates the reading contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(divi_tr_t25, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.65).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 15, 0.81).
narrative_ontology:measurement(divi_be_t25, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.71).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(divi_su_t10, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(divi_su_t25, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 25, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.14).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, egyptian_temple_economy_endowment_system).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, priestly_political_autonomy_amun_networks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel divine_legitimacy_substrate. The Atenist monotheistic reading (this story) declares legitimacy flows solely through pharaonic Aten revelation; sibling readings (amun_polytheistic_reading, folk_syncretistic_reading) declare legitimacy flows through priestly tradition or pragmatic household practice. The three readings share a referent (Egyptian divine authority) but instantiate different ε values, beneficiary/victim structures, and suppressions. All three belong to the constraint family divine_legitimacy_substrate. This reading (atenist) affects both siblings because its claim to exclusive truth directly forecloses or influences the legitimacy of alternative framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, powerless, 0.94).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
