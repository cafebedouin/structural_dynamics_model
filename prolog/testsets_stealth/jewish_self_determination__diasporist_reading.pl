% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Reading of Jewish Self-Determination: Survival Through Dispersion and Minority Rights
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   jewish_self_determination: the diasporist claim that Jewish collective
 *   survival and flourishing are best secured through diaspora pluralism and
 *   minority rights rather than territorial sovereignty, and that Zionism is
 *   a dangerous deviation binding Jewish fate to a militarized state. The
 *   constraint modeled is this doctrine as a standing arrangement in Jewish
 *   collective life — once a near-hegemonic organizing framework with mass
 *   parties, schools, and a treaty-regime strategy, now an atrophied
 *   inheritance carried by weakened institutions, academic programs, and
 *   cultural remnants. Its epsilon referent is the standing arrangement under
 *   contest — the Zionist-hegemonic organization of Jewish collective fate —
 *   priced as this reading sees it; the reading's endorsed alternative is NOT
 *   the referent. The claim/metric gap is deliberate: the doctrine is CLAIMED
 *   as piton (an atrophied alternative persisting by inertia) while the
 *   metrics are authored independently as descriptively true of its actual
 *   operation; the engine computes per-seat classifications from the
 *   structural data. KEY AGENTS (by structural relationship): -
 *   diaspora_jewish_communities: residual beneficiary
 *   ([organized]/[identity_locked]) — legitimation of distinct identity, plus
 *   exposure costs - nonzionist_diaspora_jews: primary payer
 *   ([moderate]/[constrained]) — coerced toward Zionist identification -
 *   association_endangered_jews: primary payer ([powerless]/[trapped]) — bear
 *   backlash timed to a state's actions they did not choose -
 *   diaspora_institutional_leadership: agenda_setter
 *   ([institutional]/[arbitrage]) — administers the atrophied inheritance
 *   without capturing it - zionist_hegemonic_institutions: excluded rival
 *   holder ([institutional]/[arbitrage]) — holds definitional authority this
 *   doctrine lost - host_state_pluralist_regimes: conditional guarantor
 *   ([institutional]/[analytical]) — the legal substrate the doctrine never
 *   controlled - postcolonial_jewish_studies_scholars: analytical observer
 *   ([moderate]/[analytical]) — sees the full structure
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: residual beneficiary (organized/identity_locked) — sustains distinct identity through communal institutions; receives legitimation from the pluralist framework; absorbs association backlash; exit means losing the identity infrastructure itself
 *   - nonzionist_diaspora_jews: primary payer (moderate/constrained) — decline Zionist identification; face funding exclusion and social sanction; the doctrine speaks their language but cannot shield them; exit means leaving communal life or self-censoring
 *   - association_endangered_jews: primary payer (powerless/trapped) — visible Jews bearing harassment and violence timed to Israeli military operations; exposure is a fact of birth and visibility; no individual decision exits it
 *   - diaspora_institutional_leadership: agenda_setter (institutional/arbitrage) — federation executives, seminary heads, Hillel directors administering the inheritance; allocate between local-autonomy programming and Israel-facing engagement under donor pressure; maintain the doctrine symbolically
 *   - zionist_hegemonic_institutions: excluded rival holder (institutional/arbitrage) — Israeli state agencies and Zionist umbrellas holding working definitional authority over 'Jewish interest'; absent from the doctrine's remaining deliberative spaces
 *   - host_state_pluralist_regimes: conditional guarantor (institutional/analytical) — governments and supranational bodies whose minority-rights law is the doctrine's load-bearing wall; extend or withdraw protection by domestic politics
 *   - postcolonial_jewish_studies_scholars: analytical observer (moderate/analytical) — academics articulating and archiving the doctrine; command little institutional resource
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.6).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.15).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading of Jewish Self-Determination: Survival Through Dispersion and Minority Rights").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '2ad6e79e-a55c-4a50-bfb3-78c4410ae306').
narrative_ontology:cs_kernel_codification('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', distributed).
narrative_ontology:cs_authority_grounding('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', distributed).
narrative_ontology:cs_reading_relation('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', foundational, diaspora_pluralism_secures_survival).
narrative_ontology:cs_axiom_status(diaspora_pluralism_secures_survival, holdable).
narrative_ontology:cs_axiom_grounding('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', diaspora_pluralism_secures_survival, instrumental).
narrative_ontology:cs_axiom('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', foundational, territorial_sovereignty_endangers_fate).
narrative_ontology:cs_axiom_status(territorial_sovereignty_endangers_fate, holdable).
narrative_ontology:cs_axiom_grounding('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', territorial_sovereignty_endangers_fate, empirically_contingent).
narrative_ontology:cs_axiom('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', secondary, minority_rights_regimes_as_guarantor).
narrative_ontology:cs_axiom_status(minority_rights_regimes_as_guarantor, holdable).
narrative_ontology:cs_axiom_grounding('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', minority_rights_regimes_as_guarantor, conventional).
narrative_ontology:cs_reference_frame('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', diaspora_autonomist_pluralism).
narrative_ontology:cs_drift_state('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', post_1948_sovereignty_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('2ad6e79e-a55c-4a50-bfb3-78c4410ae306', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, nonzionist_diaspora_jews).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, association_endangered_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live as religious, linguistic, and cultural minorities across dozens of host states, sustaining day schools, synagogues, presses, and mutual-aid networks that keep a distinct collective identity going without a state of their own. The pluralist-minority-rights framework gives their way of life its legitimating story and its legal defense. The same communities absorb the backlash when Israeli military actions dominate headlines, and their institutions increasingly look to Jerusalem for definitional authority, which thins the local identity the framework protects. Leaving the community would mean losing the identity infrastructure itself, so exit is rarely taken.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, payer).

% Jews who decline Zionist identification — religious universalists, Bundist descendants, secular cosmopolitans, some Orthodox anti-Zionists. Inside communal institutions they face funding exclusions, invitation bans, and social sanction pressing them to declare for Israel or accept marginality. Their doctrinal home is the pluralist tradition, but its weakened organizations cannot shield them; exit means withdrawing from Jewish communal life altogether or self-censoring within it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, nonzionist_diaspora_jews, payer,
    moderate, biographical, constrained, global).

% Visible Jews in host societies — synagogue congregants, kippah wearers, Hebrew-school children — who bear harassment and violence timed to Israeli military operations they did not choose and cannot influence. Their exposure is a fact of birth and visibility; no decision of theirs exits it. Community security budgets rise with each crisis, consuming resources from the cultural programming the pluralist framework promises.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, association_endangered_jews, payer,
    powerless, immediate, trapped, global).

% Federation executives, rabbinical seminary heads, Hillel directors, and museum boards who administer the communal institutions carrying the pluralist inheritance. They allocate budgets between local cultural-autonomy programming and Israel-facing engagement, under donor constituencies that overwhelmingly prioritize the latter. They could rebuild the local-autonomy side; the fundraising cost and donor-revolt risk exceed what they would gain, so the inheritance is maintained symbolically — heritage months, anniversary lectures — while strategic weight migrates elsewhere. Their arbitrage is framing: they can present either orientation as pluralism as donor priorities shift.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Israeli state agencies, major Zionist federations, and national umbrella organizations that hold working definitional authority over 'Jewish interest' in public life. They are not part of the pluralist tradition's remaining deliberative spaces — the Yiddishist circles, anti-Zionist minyanim, and diaspora-studies seminars where its carriers talk — and their admission would convert those spaces into auxiliaries of the sovereignty framework. Their exclusion preserves the tradition's residual autonomy while confirming its practical irrelevance.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_hegemonic_institutions, excluded,
    institutional, generational, arbitrage, global).

% National governments and supranational bodies whose minority-rights law, hate-crime statutes, and multiculturalism policies form the legal substrate the pluralist framework stands on. They extend or withdraw protection according to domestic politics and geopolitical alignment, owing the framework nothing; their tolerance is the load-bearing wall the doctrine never controlled.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_state_pluralist_regimes, observer,
    institutional, generational, analytical, continental).

% Academics and public intellectuals who articulate, archive, and defend the pluralist position — diaspora-studies programs, YIVO-affiliated historians, postcolonial theorists of Jewishness. They see the whole structure: the doctrine's history, its marginalization, its residual carriers. Their output sustains the framework's intellectual life but commands little institutional resource.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, postcolonial_jewish_studies_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of sustaining a coherent collective identity across dozens of host societies without territorial concentration: shared cultural institutions, language networks, mutual aid, and a common legal strategy of minority-rights claims let dispersed communities act as one people without triggering the dual-loyalty dynamics that sovereignty invites.
% TRANSFER_FUNCTION: Moves definitional authority over Jewish collective interest from state-centered institutions to dispersed communal and cultural bodies; moves the burden of Jewish security from a sovereign military onto host-state legal tolerance and inter-communal solidarity; in its contemporary atrophied form, moves attention and legitimacy toward a tradition whose practical levers are gone.
% ABSENT_VOICES: Jews for whom the doctrine's protection failed or was unavailable: Soviet refuseniks who exited through sovereignty-enabled routes, Ethiopian Jews airlifted by the state the doctrine disavows, Israelis who live the arrangement the doctrine condemns, and Palestinian minorities in host states whose parallel struggles the doctrine's universalism sometimes eclipses. They sit outside the doctrine's remaining deliberative circles, which are concentrated in secure Western diasporas where the cost of being wrong is lowest.
% DISAPPEARANCE_RATIONALE: The parties dispute the answer. From the carrier seats, the world rearranges: diaspora communities lose their legitimating story and legal-defense vocabulary, nonzionist Jews lose their only doctrinal home, and the academic and cultural programs sustaining the tradition dissolve. From the hegemonic seats, nothing rearranges: the sovereign arrangement and the communal mainstream continue unaffected — an asymmetry the doctrine itself laments, since its absence registers only in the corners where it still lives.
% FOUNDING_PROBLEM: How can a stateless minority scattered among often-hostile host societies survive collectively without provoking the expulsions and disloyalty accusations that followed every attempt at territorial concentration or visible national assertion — answered, in this tradition, by cultural autonomy, minority-rights treaties, and the doctrine of hereness (doykayt): building Jewish life where Jews live.
% FOUNDING_PROBLEM_CORROBORATION: The genealogy is corroborated from outside the doctrine's own carriers: historians in the YIVO tradition and academic diaspora-studies programs — institutionally independent of both Zionist federations and the doctrine's activist remnants — document its origin in the interwar minority-rights experiment and the Bund's destruction. On status, corroboration splits by seat: Israeli state institutions and mainstream federations attest the founding problem (statelessness) was solved by sovereignty and is closed; the doctrine's carriers and sympathetic historians attest the problem (diaspora vulnerability) remains live and mis-solved. No source outside the benefiting parties attests that minority-rights frameworks alone remain sufficient — that claim rests on the doctrine's own carriers, which is itself signal.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because the doctrine's contemporary costs are diffuse rather than captured: a narrowed political imagination, unshielded exposure for the association-endangered, legitimacy and attention consumed by a tradition whose practical levers are gone. No seat collects these costs — which is why gain_flow is affirmatively 'diffuse'. Suppression is low (0.15) as raw structure: the doctrine's enforcement capacity has decayed to near zero (see the falling suppression_requirement series — enforcement decay is the dynamic this story tracks, so the temporal series is authored deliberately rather than left to the scalar); what hold remains is largely internalized or carried by soft communal gatekeeping (omega: internalized_vs_structural_hold). Theater ratio is high (0.65): heritage months, anniversary lectures, and seminar discourse dominate over functional coordination — actual autonomous institutions (Bundist schools, Yiddishist press networks, treaty-advocacy machinery) are gone. Accessibility collapse is low (0.30): the doctrine does not close alternatives; it is what got collapsed upon — Zionism and hybrid positions remain fully accessible, which is precisely the atrophy signature. Resistance is moderate-high (0.55): the Zionist mainstream actively attacks the doctrine as naive or disloyal, and security-minded Jews reject it as abandoning the vulnerable. Boltzmann: identity_coordination (floor 0.08) — the doctrine coordinates boundary maintenance and membership meaning; excess extraction above the floor is real but modest and uncaptured, consistent with the claimed type rather than a capture story. Suppression is authored as raw structure and is not scaled; extractiveness is scaled by directionality and scope in the engine's computation. All three metric series run on one shared six-point grid (1897/1925/1948/1967/1991/2025) so no row substitutes an end-state scalar for an earlier value.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora-community seat the framework is a subsidy: legitimation, legal-defense vocabulary, identity depth — a low-extraction arrangement worth defending. From the nonzionist seat the same framework is a broken shield: it speaks their language but cannot protect them from communal conscription into Zionist identification. From the association-endangered seat it is a vindicated prophecy without an enforcement arm — the danger it predicted arrived, and it could do nothing. From the leadership seat it is a stewardship burden: an asset administered but neither monetizable nor safely discardable. Same-level lateral divergence is sharp: Zionist-identifying and nonzionist Jews hold comparable communal power yet experience opposite directionalities, differentiated entirely by ideological position and the exit options each position opens. The engine computes these divergent per-seat classifications from the structural data; the divergence between the subsidized beneficiary seat and the extracted payer seats is the perspectival gap this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declaration places diaspora_jewish_communities at the subsidized end, but their simultaneous exposure to association backlash and their identity-locked exit justify a directionality override to d≈0.35 — a lone beneficiary declaration would derive near-full-beneficiary and miss that the same population bears much of the harm. The two victim declarations drive nonzionist_diaspora_jews (constrained exit) and association_endangered_jews (trapped, powerless) toward the target end, the latter hardest since no decision of theirs modifies exposure; coalition potential exists (cross-community security networks, joint hate-crime advocacy) but addresses symptoms, not the structural exposure. Institutional leadership sits near-symmetric: arbitrage exit and non-capture of gains offset the administrative position — they steward the arrangement without collecting it. Zionist hegemonic institutions, though powerful, stand outside this framework's operation: they pay it nothing and it shields them not at all; their relationship is adversarial rather than extractive, which is itself signal — the doctrine no longer reaches the actors who determine Jewish fate. Global scope amplifies effective extraction modestly for the trapped seat and complicates verification for all.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a snare would require a capturer; none exists — the doctrine's costs dissipate across millions of identity-holders and no seat collects them. Classifying it as a rope would credit a coordination function that has largely ceased: the minority-rights regimes it relied on were destroyed by the same catastrophe that produced the sovereign alternative, and its remaining institutions coordinate memory, not protection. The piton classification names the actual condition: an inherited framework whose founding problem is contested, whose enforcement capacity has decayed to near zero, whose maintenance is increasingly theatrical, and which persists because burial — conceding that the Holocaust refuted its premise, rupturing communal identity, dismantling programs that anchor careers and belonging — costs more than any single bearer will pay. The agenda-setters could revive or retire it; the cost of either exceeds what they bear from leaving it alone. The classification prevents the twin mislabels: it blocks the extraction-story (no victimizer profits) and the coordination-story (the function no longer runs), locating the arrangement where the evidence puts it — mostly performance, kept alive by inertia and identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the contested kernel jewish_self_determination — the diasporist reading. Which structural features of its classification are artifacts of this reading rather than of the underlying commitment, and how would they move under the four sibling readings?',
    'Generate the sibling readings (liberal_nationalist, indigenous_return, settler_colonial, religious_covenant) as separate constraint stories and diff their beneficiary/victim sets, epsilon, and computed types against this one; convergence on structure despite divergent evaluation indicates kernel-level facts, divergence indicates reading-indexed facts.',
    'Under the liberal-nationalist reading the payer seats become beneficiaries and epsilon falls; under the settler-colonial reading epsilon rises sharply and the victim set expands beyond Jews; under the indigenous-return reading the doctrine''s central warning inverts into a warrant. The piton verdict is reading-indexed: it describes this doctrine''s position under Zionist hegemony, not the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame uncertainty: classification is indexed to one reading of a five-way contested kernel.').

omega_variable(
    internalized_vs_structural_hold,
    'Is the doctrine''s remaining hold on its adherents structural (communal gatekeeping, funding dependency, institutional career paths) or internalized (identity constituted through the pluralist self-understanding)?',
    'Post-exit trajectory study of Jews who leave diasporist milieus for Zionist-identified or unaffiliated lives: if pluralist commitments persist after gatekeeping pressure ends, the hold is substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure — adherents carry the framework with them after exit — and revival prospects rise, since identity-fused carriers can reactivate the doctrine if hegemony loosens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_hold, empirical, 'Suppression mechanism ambiguity: structural gatekeeping versus internalized identity.').

omega_variable(
    counterfactual_alternative_viability,
    'Was the atrophied alternative — functioning diasporist autonomy institutions — ever viable against twentieth-century conditions, or is the doctrine''s promise a counterfactual no achievable arrangement could have honored?',
    'Comparative historical analysis of Jewish communities under strong autonomist institutions (interwar Eastern Europe, contemporary insular enclaves) versus assimilationist or Zionist-identified communities under matched shock exposure.',
    'If the alternative was never viable, the extraction component attributable to suppressing a live alternative collapses and the classification trends toward pure inertia; if viable, the hegemonic capture of ''Jewish interest'' imposed real costs and the moderate epsilon understates the harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_alternative_viability, empirical, 'Viability of the suppressed alternative the doctrine represents.').

omega_variable(
    endangerment_attribution,
    'How much of the association-driven endangerment borne by diaspora Jews is caused by the sovereignty arrangement itself, versus baseline antisemitism that any visible Jewish collective existence would attract?',
    'Time-series comparison of anti-Jewish incident rates against Israeli military operations, controlling for baseline trend and disaggregating incidents targeting visibly non-Zionist-identifying Jews.',
    'If most endangerment is baseline, the doctrine''s central empirical wager (dispersal minimizes exposure) loses its evidentiary edge and epsilon falls; if operation-correlated, the doctrine''s victim diagnosis is vindicated and its atrophy registers as a real protection failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endangerment_attribution, empirical, 'Causal attribution of diaspora endangerment between sovereignty and baseline hostility.').

omega_variable(
    flourishing_tradeoff_weighting,
    'The doctrine trades measurable physical security for identity depth and cultural richness — how should ''flourishing'' weight these incommensurables?',
    'No dataset resolves this; it turns on whether collective life is valued primarily as survival or as meaning, a weighting each party supplies from its own values.',
    'A survival-weighted evaluation drives epsilon upward (the doctrine underdelivers its security promise); a meaning-weighted evaluation drives it downward (the doctrine delivers what it actually promises).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flourishing_tradeoff_weighting, preference, 'Preference-dependence of the flourishing standard the doctrine is judged by.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1897, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_diasp_tr_t1897, jewish_self_determination__diasporist_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(jsd_diasp_tr_t1925, jewish_self_determination__diasporist_reading, theater_ratio, 1925, 0.12).
narrative_ontology:measurement(jsd_diasp_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(jsd_diasp_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.48).
narrative_ontology:measurement(jsd_diasp_tr_t1991, jewish_self_determination__diasporist_reading, theater_ratio, 1991, 0.58).
narrative_ontology:measurement(jsd_diasp_tr_t2025, jewish_self_determination__diasporist_reading, theater_ratio, 2025, 0.65).

% Extraction over time
narrative_ontology:measurement(jsd_diasp_be_t1897, jewish_self_determination__diasporist_reading, base_extractiveness, 1897, 0.3).
narrative_ontology:measurement(jsd_diasp_be_t1925, jewish_self_determination__diasporist_reading, base_extractiveness, 1925, 0.32).
narrative_ontology:measurement(jsd_diasp_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(jsd_diasp_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(jsd_diasp_be_t1991, jewish_self_determination__diasporist_reading, base_extractiveness, 1991, 0.58).
narrative_ontology:measurement(jsd_diasp_be_t2025, jewish_self_determination__diasporist_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(jsd_diasp_su_t1897, jewish_self_determination__diasporist_reading, suppression_requirement, 1897, 0.5).
narrative_ontology:measurement(jsd_diasp_su_t1925, jewish_self_determination__diasporist_reading, suppression_requirement, 1925, 0.44).
narrative_ontology:measurement(jsd_diasp_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.38).
narrative_ontology:measurement(jsd_diasp_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.27).
narrative_ontology:measurement(jsd_diasp_su_t1991, jewish_self_determination__diasporist_reading, suppression_requirement, 1991, 0.2).
narrative_ontology:measurement(jsd_diasp_su_t2025, jewish_self_determination__diasporist_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% 'Jewish self-determination' is a colloquial label covering at least five structurally distinct constraints with different epsilons, beneficiary/victim sets, and failure modes; per the epsilon-invariance principle they are authored as separate linked stories rather than one story with a measurement parameter. This member occupies the lowest-institutional-resource position: the doctrine whose carrier institutions were destroyed and whose definitional territory was captured by the hegemonic sibling. The liberal-nationalist reading is upstream of it (its nationhood premise is shared by several siblings and is frequently cited as settled ground); the settler-colonial reading is its nearest ally in conclusion though opposed in ground (prudential-for-Jews versus justice-for-Palestinians). Edges run to all four siblings; contamination propagates along the shared nationhood premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
