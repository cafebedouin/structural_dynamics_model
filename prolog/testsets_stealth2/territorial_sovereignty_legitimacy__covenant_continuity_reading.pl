% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Covenant-Continuity Doctrine of Territorial Sovereignty Legitimacy
 *   domain: political theory/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   A legitimacy doctrine governing territorial sovereignty claims:
 *   sovereignty derives from an ancient covenant (divine grant), is carried
 *   through continuous presence across the exile generations, and is ratified
 *   by modern international instruments (Balfour Declaration 1917, League
 *   Mandate 1922, UN Resolution 181, the 1948 establishment). Under this
 *   reading the doctrine solves a real coordination problem for the claimant
 *   community, aligning religious, secular, and diaspora constituencies
 *   behind one title narrative, while asymmetrically subordinating the rival
 *   claimant population, whose counter-claims the doctrine rules out a priori
 *   by assigning them no standing under pre-existing title. Per the
 *   epsilon-invariance principle this story decomposes the
 *   sovereignty-legitimacy label: it instantiates ONLY the
 *   covenant_continuity_reading; the self_determination_reading and
 *   existential_matrix_reading are separate constraints linked via the
 *   network. The claim/metric relationship is deliberate and unreconciled:
 *   the constraint is CLAIMED as tangled_rope (genuine coordination plus
 *   asymmetric extraction), while the authored extractiveness is
 *   reading-indexed, meaning the covenant-continuity frame itself scores the
 *   standing arrangement's extraction low because it reads the arrangement as
 *   restored justice rather than dispossession. The engine computes per-seat
 *   classifications from the structural data; the divergence between the
 *   frame's self-assessment and the payer seats' computed experience is the
 *   measurement this story exists to take.
 *
 * KEY AGENTS:
 *   - - israeli_state_institutions: Agenda-setter (institutional/identity_locked) — administers, codifies, and diplomatically defends the doctrine; collects its legitimacy product
 *   - - religious_zionist_settlement_movement: Primary beneficiary with secondary agenda-setting (organized/identity_locked) — converts doctrine into land access and funding
 *   - - diaspora_zionist_institutions: Secondary beneficiary (organized/constrained) — collects mobilization and identity continuity
 *   - - palestinian_west_bank_residents: Primary payer (powerless/trapped) — bears administration and land loss without franchise
 *   - - palestinian_refugee_descendants: Payer across generations (powerless/constrained) — bears subordinated return claims
 *   - - palestinian_arab_citizens_of_israel: Payer inside the polity (moderate/constrained) — bears the encoded hierarchy
 *   - - israeli_secular_liberals: Dual-positioned beneficiary/payer (organized/mobile) — collects state legitimacy, pays censure and coalition costs
 *   - - palestinian_rejectionist_factions: Excluded voice (organized/trapped) — denied a seat by the frame's own semantics
 *   - - international_legal_community: Analytical observer (institutional/analytical) — produces the contrary-instrument record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.3).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Covenant-Continuity Doctrine of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political theory/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'b46e7239-fa4a-4701-839b-d4074d14b154').
narrative_ontology:cs_kernel_codification('b46e7239-fa4a-4701-839b-d4074d14b154', fixed_text).
narrative_ontology:cs_authority_grounding('b46e7239-fa4a-4701-839b-d4074d14b154', lineage).
narrative_ontology:cs_interpretation_layer_present('b46e7239-fa4a-4701-839b-d4074d14b154').
narrative_ontology:cs_reading_relation('b46e7239-fa4a-4701-839b-d4074d14b154', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('b46e7239-fa4a-4701-839b-d4074d14b154', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('b46e7239-fa4a-4701-839b-d4074d14b154', foundational, divine_grant_confers_enduring_title).
narrative_ontology:cs_axiom_status(divine_grant_confers_enduring_title, holdable).
narrative_ontology:cs_axiom_grounding('b46e7239-fa4a-4701-839b-d4074d14b154', divine_grant_confers_enduring_title, theological).
narrative_ontology:cs_axiom('b46e7239-fa4a-4701-839b-d4074d14b154', foundational, recognition_ratifies_preexisting_right).
narrative_ontology:cs_axiom_status(recognition_ratifies_preexisting_right, holdable).
narrative_ontology:cs_axiom_grounding('b46e7239-fa4a-4701-839b-d4074d14b154', recognition_ratifies_preexisting_right, conventional).
narrative_ontology:cs_reference_frame('b46e7239-fa4a-4701-839b-d4074d14b154', covenant_grant_unbroken_title).
narrative_ontology:cs_drift_state('b46e7239-fa4a-4701-839b-d4074d14b154', contemporary_post_icj_advisory_opinion, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b46e7239-fa4a-4701-839b-d4074d14b154', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_zionist_institutions).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_west_bank_residents).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_descendants).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_secular_liberals).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_secular_liberals).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, divine_land_grant_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, continuous_presence_title_principle).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, recognition_instruments_as_ratification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the state whose founding account this doctrine supplies: teaches the covenant-continuity narrative in the school curriculum, maintains the archaeological and ceremonial apparatus that stages it, codified its terms in the 2018 Nation-State Basic Law, and defends it in every diplomatic forum. Collects the doctrine's principal product, an unbroken-title narrative that anchors sovereignty claims against demographic and legal challenge. Renouncing the doctrine would require the state to disown its own founding account; its institutions treat that as unavailable regardless of cost.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Supplies the doctrine's most active interpreters and enforcers: yeshiva networks, settlement councils, and youth movements that translate the covenant into land policy. Receives doctrinal warrant for building beyond the 1949 lines, since under this account settlement is return rather than colonization, along with state funding and legal protection. Members' careers, marriages, and residences are constituted inside the covenant frame; leaving it would dissolve the community's reason for being where it is.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement, agenda_setter).

% Federations, synagogue movements, and advocacy organizations that fund and defend the state partly through the covenant-continuity account, which links dispersed communities to the territory across the exile generations. Collects mobilization energy and identity continuity from the narrative. Openly abandoning the account risks communal schism and loss of the institutional base, so exit is costly even where doubts exist.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_zionist_institutions, beneficiary,
    organized, generational, constrained, global).

% Live under military administration whose continuation the doctrine frames as temporary security management over land already held by prior right. Bear checkpoint regimes, land requisitions for settlement roads and outposts, and planning denials; under the doctrine's semantics their communities occupy someone else's restored inheritance rather than a competing national home. Cannot vote for the government administering them and cannot realistically relocate.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_west_bank_residents, payer,
    powerless, biographical, trapped, regional).

% Inherit registered refugee status across three to four generations in camps and host states. The doctrine's continuity-of-title semantics subordinates their return claims: if title never lapsed, return is framed as migration into a restored estate rather than homecoming. Their claim survives mainly as a negotiating chip the doctrine discounts; absorption into full host-state citizenship remains partial and contested.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_descendants, payer,
    powerless, generational, constrained, global).

% Hold formal citizenship and parliamentary representation inside a state whose basic law defines itself around the covenant-continuity account. Bear the hierarchical position the account encodes: state symbols, land policy, and immigration law privilege the returning claimant community. Emigration is possible but severs family and community; staying means advocating inside a frame that assigns their collective a subordinate chapter.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Draw civic identity and security from the state the doctrine legitimates while holding the recognition pillars (Balfour, UN Resolution 181, 1948) more firmly than the covenant pillar. Benefit from the stability the narrative provides; bear the international censure, coalition dependence on settler parties, and internal polarization its theological strand generates. Emigration is a live option for the young; most stay and argue.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_secular_liberals, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_secular_liberals, payer).

% Groups that refuse recognition of the state outright and are therefore locked out of every conversation the doctrine structures, including negotiation tracks, recognition diplomacy, and academic exchange. The exclusion is self-chosen in form and structural in effect: the doctrine's frame has no seat for parties who deny its premise, so their objection is never heard where it would carry weight.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_rejectionist_factions, excluded,
    organized, biographical, trapped, regional).

% Court judges, UN special rapporteurs, treaty bodies, and international-law scholars who test the doctrine's claims against charter principles, the Geneva conventions, and advisory opinions. Produce the contrary-instrument record (Security Council resolutions 242 and 2334, the 2024 ICJ advisory opinion) that the doctrine must absorb or dispute. Hold no stake in the outcome beyond doctrinal consistency.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns a geographically dispersed claimant community, spanning religious, secular, and diaspora constituencies, behind a single title narrative, solving the collective-action problem of sustaining one national claim across exile generations and across groups with otherwise divergent priorities.
% TRANSFER_FUNCTION: Moves legitimacy-recognition and land-access toward the covenant-continuity claimant side and away from the rival claimant population: settlement land, planning permissions, and diplomatic capital flow under the doctrine's warrant, while the rival population's counter-claims are moved out of the adjudicable set entirely.
% ABSENT_VOICES: Voices holding the self-determination premise are structurally outside the doctrine's internal adjudication, since the frame assigns them no standing rather than answering them; rejectionist factions are doubly excluded, by others and by their own refusal; critical archaeologists and historians whose findings complicate the continuity account are marginalized in state curricula. Their objections surface only in external forums (UN bodies, foreign courts, the ICJ) that the doctrine's holders discount as biased.
% DISAPPEARANCE_RATIONALE: Overnight removal would force the claimant coalition to re-found its claim on modern-legal grounds alone: the settlement enterprise loses its return framing and faces straightforward colonization charges, coalition politics reorganize around the recognition pillars alone, diaspora mobilization loses its trans-generational hook, and negotiations proceed on purely demographic-partition terms. The dispute's entire grammar changes.
% FOUNDING_PROBLEM: A national movement claiming a territory in which it was a demographic minority needed a title that outranked headcounts: the doctrine bridges an ancient documentary-theological grant to modern interstate recognition so the claim survives its arithmetic disadvantage.
% FOUNDING_PROBLEM_CORROBORATION: External instruments attest the problem the doctrine answers: the Balfour Declaration and the League Mandate were issued precisely because the claimant population lacked local majority standing, and Resolution 181's partition logic concedes the demographic arithmetic. Adversarial corroboration exists too: Arab-state rejection documents and Palestinian leadership communications engaged the recognition track rather than ignoring it, implicitly conceding the legitimacy deficit was real. No source outside the benefiting parties attests that the deficit is closed, which is why the status is live.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).
:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.30 as a reading-indexed value: the covenant-continuity frame assesses the standing arrangement as substantially just implementation of prior right, conceding real burdens on the rival population (occupation administration, land requisitions) but attributing them to security necessity and rejected compromises rather than to the doctrine's operation. The structural declarations tell the other half: named payers exist, and the engine amplifies effective extraction for trapped targets regardless of the frame's self-assessment. Suppression (0.68) is a raw, unscaled structural property: the doctrine actively excludes rival legitimacy frameworks from official adjudication, marginalizes complicating historiography in state curricula, and requires continuous institutional maintenance (curriculum, ceremony, legislation, diplomacy) to hold; only extractiveness is scaled by directionality and scope, never suppression. Theater ratio (0.35) reflects genuine functional content, since the doctrine organizes education, law, and mobilization, alongside a growing ceremonial layer of archaeological pageantry and anniversary ritualization. Accessibility collapse is LOW (0.35): the rival readings remain fully elaborated and live, and understanding this doctrine closes off neither the self-determination nor the existential frame. Resistance is HIGH (0.80): the doctrine meets sustained counter-instrument production, boycott movements, and armed and diplomatic contestation. The measurement series share one seven-point grid; the 1993 dip in suppression_requirement records the Oslo devolution's temporary lowering of direct enforcement, reversed after 2000 — a ratchet with one relaxation, not a cycle. Identity-lock dynamics operate at three levels: the state's fusion is institutional (its organs have become the doctrine's administrators), the settlement movement's is ideological-theological (exit dissolves the community's constituting premise), and the diaspora institutions' is relational-communal. Coalition note: the three payer seats are differentiated by status (residents, refugees, citizens), and the doctrine's categorical distinctions fragment any joint payer coalition, which is itself stabilizing for the arrangement. Boltzmann gaming alert: identity_coordination framing is a common cover for extraction; here the identity function is genuine for the in-group, but the Power x Scope coupling concentrates extraction on powerless agents at regional-to-global scope, which the complexity offset does not excuse.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats the same structure computes as restored justice and identity infrastructure, a coordination achievement whose remaining costs are attributed to others' rejection. From the payer seats it computes as a priori foreclosure: a doctrine that assigns their communities the status of occupants of someone else's restored inheritance before any negotiation begins. West Bank residents experience the arrangement as administration without franchise; refugee descendants as inherited dispossession; Arab citizens as licensed membership in someone else's homecoming. The engine computes these per-seat types from power, exit, and directionality; the divergence between the frame's self-scored low extraction and the payer seats' computed high effective extraction (amplified by trapped exit and identity-locked beneficiaries) is the datum this story contributes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map toward the beneficiary end: state institutions collect the title narrative's legitimacy product, the settlement movement converts doctrine into land access and funding, and diaspora institutions convert it into mobilization. Victim declarations map toward the target end: residents bear administration and land loss with trapped exit, refugee descendants bear subordinated return claims across generations, and Arab citizens bear the encoded hierarchy with constrained exit. Secular liberals sit near symmetric, collecting state legitimacy while paying censure and coalition costs. No directionality overrides are authored: the derivation from role declarations plus exit options reproduces these positions, and the coarse power-atom keying of overrides would misapply any correction across same-power seats (the moderate atom covers both Arab citizens and secular liberals, whose directionalities differ sharply).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, grounding a minority-population national claim in a territory where modern self-determination logic favored the other side, remains live: the legitimacy contest is unresolved, the recognition instruments are actively contested, and the doctrine's enforcement budget has grown across the whole interval. Nothing here is vestigial; the mandate has not outlived its function and no sunset exists. The tangled_rope classification performs the anti-mislabeling work in both directions: a pure-extraction reading would erase the doctrine's genuine coordination function, since removing it really would fragment the claimant coalition across its religious, secular, and diaspora strands; a pure-coordination reading would erase the asymmetric subordination the same structure imposes on the rival population. It also guards the subtler error: because the frame self-scores extraction low, a naive consumer of its self-assessment would certify benign coordination, and the named payers plus the enforcement requirement prevent that certification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the covenant_continuity_reading of the kernel territorial_sovereignty_legitimacy; the sibling readings (self_determination_reading, existential_matrix_reading) instantiate different constraints over the same territory with different claimant sets, temporal scopes, and victim structures. Which reading governs adjudication is the deepest open variable in this story.',
    'Cross-reading comparison once the sibling stories are compiled: identical structural probes (who bears costs, what vanishes on disappearance) run against each reading''s file; divergent outputs localize the disagreement to specific structural elements.',
    'If the self_determination_reading governed, the victim set inverts (the settlement enterprise becomes the arrangement extracted upon) and temporal scope contracts to the modern period; if the existential_matrix_reading governed, juridical metrics drop out entirely and the structure becomes force-ordered rather than doctrine-ordered. Every classification in this file is indexed to the covenant-continuity frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: this story''s values hold only within the covenant-continuity frame.').

omega_variable(
    demographic_absence_title_survival,
    'Can a legitimacy claim grounded in ancient residence survive roughly eighteen centuries of demographic minority or absence, or does title lapse as most modern property and sovereignty regimes presume?',
    'Comparative doctrine analysis: how other restorative-title regimes (post-colonial restitution, historic-property claims) treat multi-generational absence; whether any functioning legal system honors title surviving comparable discontinuity.',
    'If absence lapses title, the covenant pillar reduces to cultural memory and the reading''s weight shifts entirely onto the recognition instruments; temporal scope contracts and the extraction profile converges toward the self-determination frame''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_absence_title_survival, conceptual, 'Whether the continuity pillar survives the exile interlude.').

omega_variable(
    recognition_instrument_status,
    'Do the Balfour Declaration, the League Mandate, and UN Resolution 181 constitute binding grants of title, or political endorsements that later instruments (Security Council resolutions 242 and 2334, the 2024 ICJ advisory opinion) can qualify or revoke?',
    'International-law analysis of the instruments'' normative status and of subsequent Security Council and ICJ treatment; state-practice evidence on whether the recognition is treated as revocable.',
    'If revocable, the modern-recognition pillar cannot independently carry the claim and the doctrine rests wholly on the theological pillar, raising its dependence on identity enforcement; if binding, the reading''s juridical floor strengthens and external contestation reads as revision rather than correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_instrument_status, empirical, 'Normative weight of the recognition instruments versus later contrary instruments.').

omega_variable(
    partition_compromise_semantics,
    'Did the partition instruments create sovereign entitlements where none previously existed (the sibling self-determination frame), or did they compromise a pre-existing right (this reading''s frame)? The two semantics assign opposite directions to every downstream settlement question.',
    'Textual and drafting-history analysis of Resolution 181, plus comparison of how the parties themselves characterized the instrument at acceptance and at rejection.',
    'Under the creation semantics, post-partition expansion lacks warrant and this reading''s settlement framing collapses; under the compromise semantics, the 1949-67 lines were always provisional and the return framing holds. This is the sharpest disagreement locus between this reading and the self_determination_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_compromise_semantics, conceptual, 'Whether partition created or compromised right, the pivotal semantic fork.').

omega_variable(
    ingroup_suppression_mechanism,
    'Within the benefiting population, is dissent from the doctrine suppressed structurally (institutional penalties, funding exclusion, coalition expulsion) or internalized (identity fusion renders exit unthinkable before any penalty applies)?',
    'Post-exit trajectory tracking of public figures who renounced the doctrine: if penalties follow renunciation, the mechanism is structural; if renunciants report no penalty but report a prior inability to imagine exit, the mechanism is internalized.',
    'If predominantly internalized, the measured suppression understates the doctrine''s hold, since enforcement machinery is smaller than the lock-in it produces; if structural, enforcement capacity is the binding mechanism and its growth trajectory drives persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ingroup_suppression_mechanism, empirical, 'Structural versus internalized suppression split within the beneficiary population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement_basis(terr_tr_t1917, observed).
narrative_ontology:measurement(terr_tr_t1947, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1947, 0.18).
narrative_ontology:measurement_basis(terr_tr_t1947, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1980, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement_basis(terr_tr_t1980, observed).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement_basis(terr_tr_t1993, observed).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement_basis(terr_tr_t2005, observed).
narrative_ontology:measurement(terr_tr_t2025, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(terr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.1).
narrative_ontology:measurement_basis(terr_be_t1917, observed).
narrative_ontology:measurement(terr_be_t1947, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1947, 0.18).
narrative_ontology:measurement_basis(terr_be_t1947, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1980, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1980, 0.26).
narrative_ontology:measurement_basis(terr_be_t1980, observed).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.24).
narrative_ontology:measurement_basis(terr_be_t1993, observed).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2005, 0.27).
narrative_ontology:measurement_basis(terr_be_t2005, observed).
narrative_ontology:measurement(terr_be_t2025, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2025, 0.3).
narrative_ontology:measurement_basis(terr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.15).
narrative_ontology:measurement_basis(terr_su_t1917, observed).
narrative_ontology:measurement(terr_su_t1947, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1947, 0.25).
narrative_ontology:measurement_basis(terr_su_t1947, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1980, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(terr_su_t1980, observed).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement_basis(terr_su_t1993, observed).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement_basis(terr_su_t2005, observed).
narrative_ontology:measurement(terr_su_t2025, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement_basis(terr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% Territorial sovereignty legitimacy decomposes per the epsilon-invariance principle into three structurally distinct constraints sharing one kernel: this covenant-continuity reading (theological-documentary title surviving demographic absence; extraction self-scored low by its own lights), the self_determination_reading (modern demographic-majority title; victim set inverted relative to this story), and the existential_matrix_reading (force-structured survival logic; juridical metrics inapplicable). The influence gradient runs outward from this reading: its temporal-depth claim is cited against the self-determination frame's recency, and the existential frame cites both frames' failure as evidence that legality is epiphenomenal. Each story carries its own epsilon, beneficiaries, and victims; cross-reading comparison localizes the disagreement to temporal scope, partition semantics, and demographic override.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
