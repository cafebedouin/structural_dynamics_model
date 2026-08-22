% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country Two Systems — Sovereignty Primacy Reading (Security Override over Hong Kong Autonomy)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   Hong Kong's constitutional order since 1997 rests on a contested kernel:
 *   the One Country, Two Systems framework, declared jointly in 1984 and
 *   codified in the Basic Law. This story instantiates one reading of that
 *   kernel — the sovereignty-primacy reading — under which Hong Kong's
 *   autonomy is a delegation from PRC sovereign authority, held revocably,
 *   with national security and territorial integrity lexically prior to local
 *   autonomy wherever they conflict. The reading's structural signature is
 *   the 2020 National Security Law annexed into the Basic Law over local
 *   objection, the Office for Safeguarding National Security operating
 *   mainland personnel inside the territory outside local jurisdiction, the
 *   2021 patriots-only electoral overhaul, and a judiciary that administers a
 *   parallel security regime without juries or ordinary bail rules. The claim
 *   and the metrics are authored independently: claimed_type is tangled_rope
 *   because the arrangement retains a genuine coordination core
 *   (incorporation of the territory under unified sovereignty — a real
 *   problem the 1980s settlement had to solve) while operating with heavy
 *   asymmetric extraction; the metrics describe the arrangement's actual
 *   operation, which is substantially extractive and actively enforced.
 *   Divergence between the claim and the engine's computed seat-level types
 *   is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - prc_central_authorities: Primary beneficiary and agenda-setter (institutional/arbitrage) — collects final authority, controls interpretation and amendment, imposed the security statute
 *   - mainland_security_organs: Enforcement beneficiary (institutional/arbitrage) — operates the security machinery inside the territory outside local jurisdiction
 *   - pro_beijing_hk_establishment: Secondary beneficiary (organized/identity_locked) — holds offices under the vetted electoral system that excluded its rivals
 *   - hk_pro_democracy_citizens: Primary target (moderate/constrained) — bears speech, assembly, and association costs; class-stratified exit
 *   - hk_opposition_politicians: Primary target (moderate/trapped) — disqualified, prosecuted, dissolved, imprisoned, or exiled
 *   - hk_independent_journalists: Primary target (moderate/constrained) — newsrooms closed by prosecution and asset freeze
 *   - hk_civil_society_organizations: Primary target (moderate/trapped) — unions and advocacy bodies deregistered or disbanded
 *   - hk_judiciary: Dual-positioned payer-beneficiary (institutional/identity_locked) — administers the security regime to preserve the institution
 *   - hk_business_elites: Mixed beneficiary-payer (powerful/arbitrage) — stability and market access against override risk
 *   - hk_youth_generation: Primary target (powerless/trapped) — inherits the constraint with the least capacity to leave
 *   - foreign_businesses_in_hk: Mixed payer-beneficiary (powerful/mobile) — platform benefits against compliance and decoupling exposure
 *   - joint_declaration_signatories: Excluded guarantor (institutional/constrained) — declares breach, holds no enforcement seat
 *   - un_treaty_bodies: Analytical observer (institutional/analytical) — catalogues covenant conflicts, no enforceable lever
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.85).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country Two Systems — Sovereignty Primacy Reading (Security Override over Hong Kong Autonomy)").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'e2132225-b0db-4883-84c0-b6124aad2e2e').
narrative_ontology:cs_kernel_codification('e2132225-b0db-4883-84c0-b6124aad2e2e', fixed_text).
narrative_ontology:cs_authority_grounding('e2132225-b0db-4883-84c0-b6124aad2e2e', extraction).
narrative_ontology:cs_interpretation_layer_present('e2132225-b0db-4883-84c0-b6124aad2e2e').
narrative_ontology:cs_reading_relation('e2132225-b0db-4883-84c0-b6124aad2e2e', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e2132225-b0db-4883-84c0-b6124aad2e2e', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('e2132225-b0db-4883-84c0-b6124aad2e2e', foundational, autonomy_is_delegated_not_inherent).
narrative_ontology:cs_axiom_status(autonomy_is_delegated_not_inherent, holdable).
narrative_ontology:cs_axiom_grounding('e2132225-b0db-4883-84c0-b6124aad2e2e', autonomy_is_delegated_not_inherent, conventional).
narrative_ontology:cs_axiom('e2132225-b0db-4883-84c0-b6124aad2e2e', foundational, national_security_lexically_prior_to_local_autonomy).
narrative_ontology:cs_axiom_status(national_security_lexically_prior_to_local_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('e2132225-b0db-4883-84c0-b6124aad2e2e', national_security_lexically_prior_to_local_autonomy, instrumental).
narrative_ontology:cs_reference_frame('e2132225-b0db-4883-84c0-b6124aad2e2e', delegated_autonomy_with_sovereign_reserve_powers).
narrative_ontology:cs_drift_state('e2132225-b0db-4883-84c0-b6124aad2e2e', post_nsl_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e2132225-b0db-4883-84c0-b6124aad2e2e', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_organs).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, pro_beijing_hk_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_citizens).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_opposition_politicians).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_independent_journalists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_elites).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, foreign_businesses_in_hk).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_elites).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_youth_generation).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, foreign_businesses_in_hk).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, npc_comprehensive_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, delegated_autonomy_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, comprehensive_national_security_concept).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the Basic Law, retains its interpretation and amendment through the NPCSC, annexed the National Security Law into the Basic Law's Annex III in 2020 to bypass local legislation, issues white papers asserting comprehensive jurisdiction, and directs Hong Kong policy through central coordination bodies. Collects the arrangement's principal gains: final political authority over the territory, security jurisdiction, and the dismantling of a political opposition it designates a sovereignty threat. No external institution reviews or binds its constitutional acts.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Operate the Office for Safeguarding National Security in Hong Kong under PRC law and personnel, outside Hong Kong jurisdiction: they receive case referrals, exercise investigative powers, and in designated cases take over prosecution entirely. Their presence inside the territory is new since 2020 — enforcement agents of the sovereign now operate where none previously stood, and they are immune from local legal process. They collect expanded jurisdiction and institutional presence while administering the machinery they enforce.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_organs, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_organs, beneficiary).

% Hold legislative, executive, and advisory positions under the patriots-only electoral system installed in 2021, which vets candidates for loyalty and has excluded the opposition entirely. They collect offices, policy influence, and state-aligned business advantages. Their standing is inseparable from the arrangement: the vetting system that seats them is the same machinery that disqualifies rivals, and defection would cost them both position and identity.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, pro_beijing_hk_establishment, beneficiary,
    organized, biographical, identity_locked, regional).

% Benefit from stability, mainland market access, and the suppression of the labor and political disruption of 2019, and many hold advisory seats under the restructured electoral bodies. They pay in eroded rule-of-law guarantees: contract enforcement, data privacy, and asset security now carry sovereign-override risk. Capital and second residencies are movable; the Hong Kong franchise is not.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_elites, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_elites, payer).

% Bear the arrangement's principal costs: political speech, assembly, publication, and association now carry national-security exposure, books are pulled from shelves, commemorations are criminalized, and the 2019-2020 protest wave ended in mass arrests. Emigration exists — tens of thousands left under visa schemes — but it is costly, class-stratified, and means abandoning home, property, and family; those who stay live under the constraint with no political channel.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_citizens, payer,
    moderate, biographical, constrained, regional).

% Faced a closing sequence: disqualification from councils and the legislature, prosecution of the 47 democrats over an organized primary, party dissolutions, and imprisonment or exile for senior figures. Their organizations are dismantled; re-entry into electoral politics requires passing loyalty vetting their own records fail. Exit abroad forfeits the constituency entirely.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_opposition_politicians, payer,
    moderate, biographical, trapped, regional).

% Operated the territory's last open press: the largest independent newspaper was closed by asset freeze and prosecution of its executives, and two further outlets shut after editor arrests. Remaining newsrooms practice pre-clearance caution; journalists avoid sovereign topics. Exit means exile publications that lose their audience and revenue inside the territory.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_independent_journalists, payer,
    moderate, biographical, constrained, regional).

% Trade unions, religious bodies, professional associations, and advocacy groups that formed the territory's associational fabric: the largest unions disbanded under asset and membership risk, the main protest coalition dissolved after a funding investigation, charities were deregistered. Leaders face arrest exposure; reconstitution requires registration the security regime can veto. Exit means dissolving or relocating abroad with no local legal personality.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society_organizations, payer,
    moderate, biographical, trapped, regional).

% Retains common-law forms and continues ordinary civil and commercial adjudication, which sustains the territory's legal-services economy and the judiciary's own institutional standing. In national security cases it administers a different regime: designated judges, no jury, reversed bail presumptions, and a Beijing-side supervisory committee that can take cases over. Senior judges describe administering the regime as defending judicial independence; several overseas judges have resigned rather than sit in it.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary, payer,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary, beneficiary).

% Came of age during the protest years and now face national security education in schools, vetted curricula, criminal records for protest participation, and an electoral system in which no candidate represents opposition politics. The emigration wave has taken many families' breadwinners; those without resources or visas inherit the constraint with the least capacity to leave.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_youth_generation, payer,
    powerless, biographical, trapped, regional).

% Benefit from the territory's financial infrastructure and China-market adjacency, and from the suppression of the political disruption that preceded 2020. They pay in compliance exposure: national security data demands, staff speech risk, and the possibility that sanctions or decoupling render the platform unusable. Regional-headquarters relocations have begun; the option is real and unevenly exercised.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, foreign_businesses_in_hk, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, foreign_businesses_in_hk, beneficiary).

% The United Kingdom and other states that underwrote the 1984 Joint Declaration hold that the sovereignty-primacy turn breaches its terms and have said so in formal assessments. They possess no enforcement mechanism inside the arrangement — no standing before Hong Kong courts, no role in the Basic Law's interpretation. Their available instruments are declaratory: visa schemes, sanctions designations, diplomatic protest.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, joint_declaration_signatories, excluded,
    institutional, generational, constrained, global).

% Human Rights Committee and special procedures review Hong Kong's compliance with the ICCPR as applied through the Joint Declaration and Basic Law, and have catalogued the national security regime's conflicts with covenant rights. They compile findings and issue recommendations; none are enforceable against the sovereign, and the state party declines their jurisdictional premises.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, un_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authorities).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one sovereign state: unified foreign policy, defense, and territorial integrity over a territory with a distinct legal and economic system, incorporating a former colony without institutional merger or capital-flight collapse. The delegation structure gives the center final authority while the local system runs daily administration.
% TRANSFER_FUNCTION: Moves final political authority and security jurisdiction from Hong Kong institutions and residents to the PRC central authorities; moves enforcement capacity into the territory (security statute, mainland personnel, vetting bodies); moves political participation itself into a loyalty-vetted channel, transferring office-holding from elected opposition to vetted loyalists.
% ABSENT_VOICES: The voters who gave the pro-democracy camp its majorities in the 2019 district elections, imprisoned and exiled opposition figures, shuttered newsrooms, dissolved unions and civil society bodies, and the Joint Declaration's guarantor governments all lack a seat in the arrangement's current administration. They would contest the premise that the security override serves the territory's inhabitants; the conversation that now defines the constraint includes only seats the vetting system has approved.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primacy structure — the revocability doctrine, the security override, the annexed security statute — vanished overnight, the arrangement would rearrange immediately: security prosecutions would lose their legal basis, the patriots-only vetting would have no warrant, mainland security personnel would be present without law, disqualified and exiled politicians would have a path back, and the Basic Law's autonomy chapter would revert to its enforceable-guarantee or negotiated readings. The territory's political order is currently organized around this reading; its removal forces reorganization.
% FOUNDING_PROBLEM: The 1980s problem of resuming sovereignty over a capitalist colony whose prosperity, legal system, and international ties Britain would hand over: how to incorporate the territory without triggering institutional collapse or capital flight, while preventing it from becoming a base for subversion of the socialist state. The Joint Declaration and Basic Law were built to manage that transition.
% FOUNDING_PROBLEM_CORROBORATION: The benefiting parties (central authorities, Hong Kong government) attest the security problem is live and permanent, citing subversion and foreign interference. Outside corroboration runs the other way: UK government assessments and UN treaty-body findings describe the transition-era problem as solved and the current security rationale as operating to dismantle the autonomy the founding documents promised; the composition of prosecutions (expression, association, and publication rather than violence) is cited by outside legal analysts against the threat magnitude the override presumes. No corroborator outside the benefiting parties attests that the founding problem remains live in the form the override requires; several outside sources attest it was resolved by the transition's completion.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: political speech, assembly, publication, and association carry national-security exposure; most prosecuted cases arise from expression and organizing rather than violence; the electoral channel has been closed to opposition by vetting. Suppression 0.85 exceeds extractiveness because persistence does not rest on participant consent: it rests on a statute the local legislature could not have passed, enforcement agents outside local jurisdiction, reversed bail presumptions, and candidate vetting. Theater 0.6: the 'high degree of autonomy' and 'two systems' framing is still officially maintained and performs continuity to international audiences while the override governs the politically salient cases. Accessibility collapse 0.72: within the territory political alternatives have collapsed almost completely (dissolved parties, vetted elections, shuttered press); exit exists but is class-stratified and costly, which keeps the value below the near-total collapse of a natural limit. Resistance 0.5: the 2019-2020 wave mobilized up to two million marchers and was met with mass arrest; open resistance is now minimal, but the constraint continues to meet international censure, diaspora advocacy, and quiet noncompliance. Suppression here is predominantly structural — statute, prosecution, vetting — with a growing internalized layer (pre-clearance self-censorship, curriculum effects) that would persist if enforcement relaxed. All three temporal series run on one shared eight-point grid. The dynamics are a ratchet rather than a cycle: each mobilization wave (2003, 2014, 2019) was answered with a machinery build-out that left the baseline higher — attempted local security legislation, the 2014 electoral decision, then the annexed security statute — so the series step up at 2020 rather than oscillating; the ratchet itself is the extraction mechanism, converting each round of resistance into permanent capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (prc_central_authorities) the arrangement computes as sovereign coordination the center built and maintains: security is defined so broadly that its enforcement is definitionally protective, and revocability is not experienced as a cost because the center holds the revocation power. From the payer seats the same structure operates as enforced removal of political liberty without a compensating voice. The judiciary seat is the sharpest divergence: administering the security regime is experienced internally as preserving the institution's existence and the common-law economy that depends on it, while its structural effect is to extend the override into adjudication. Inter-institutionally, the same constraint reaches institutional actors with different exits: the center holds arbitrage (it authored the rules and can rewrite them), the judiciary is identity-locked (its common-law identity is fused with administering the system that constrains it), and the treaty guarantors are reduced to declaratory protest. Same-level divergence: among moderate-power Hong Kong seats, politicians are trapped (their records fail vetting; exit forfeits the constituency), journalists are constrained (exile costs the audience), and citizens are constrained-but-mobile-at-a-price — equal nominal standing, different exits, because the constraint binds each through a different asset: office, audience, home.
 *
 * DIRECTIONALITY LOGIC:
 *   The center and its security organs are declared beneficiaries and sit near the beneficiary end: they collect final authority, jurisdiction, and the suppression of a designated opposition, and neither can be bound by the arrangement they administer. The pro-Beijing establishment is a beneficiary whose position is constituted by the vetting machinery — the benefit is real but identity-locked to the arrangement's persistence. Declared victims — pro-democracy citizens, opposition politicians, independent journalists, civil society organizations — sit near the full-target end, with trapped and constrained exits keeping their effective extraction near the unscaled maximum. The judiciary is dual-positioned: payer in security matters, beneficiary in the ordinary commercial role the arrangement preserves. Business seats (HK elites, foreign firms) are mixed: real coordination benefits, real override exposure, arbitrage or mobile exit that only partly prices the risk. A directionality override pins the powerful atom at 0.38 because the structural derivation would read both powerful seats as beneficiaries and drive them toward the subsidized end; their exposure (asset security, data demands, sanctions risk) is genuine and only partly hedgeable, so 0.38 keeps them mixed rather than subsidized. Spatial scope is regional for the territory-bound seats and global for the center, the businesses, and the treaty parties — the engine's scope amplification falls hardest on the trapped regional targets, whose verification-resistant environment is the point of the design.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement's founding problem — managing the incorporation of a capitalist enclave into a socialist state without institutional collapse — was substantially solved by the transition's completion; what persists is an expanded, permanently-asserted security rationale. The tangled_rope claim prevents two mislabels at once: reading the structure as pure extraction ignores the real coordination core (unified sovereignty over the territory is a function any settlement must perform, and the ordinary legal-commercial system still runs under it), while reading it as pure coordination ignores the measured accumulation — theater_ratio rising 0.20 to 0.60 and extractiveness 0.38 to 0.78 across the interval show the extraction share growing relative to function. The structure is drifting snare-ward: if the security function becomes wholly theatrical while the override machinery persists, the correct classification flips and mandatrophy should be declared. It is not declared here because the founding parties assert the problem live and the R5 corroboration records that assertion as contested rather than resolved — the dispute is the datum, not an obstacle to it. The kernel frame matters for this analysis: under the autonomy-primacy sibling, the same arrangement computes as pure extraction with no coordination claim available to it; the tangled_rope classification is a property of this reading's own structure, not of the topic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the sovereignty-primacy reading the kernel''s actual content, or one of three structurally distinct claims enforced into dominance?',
    'Comparative analysis of the Joint Declaration''s text and UN registration, the Basic Law drafting record, and the NPCSC interpretation sequence against the two sibling readings; the treaty''s international registration anchors the autonomy-primacy side, the 1982 Constitution''s Article 31 and NPC interpretation power anchor the sovereignty side.',
    'Under the autonomy-primacy sibling as governing constraint, the security machinery and vetting system are ultra vires and this story''s epsilon collapses toward the siblings''; under the balanced-coexistence sibling, the override is bounded by negotiation duties and the extraction is partial. This story''s classification holds only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the One Country Two Systems kernel governs, and whether dominance reflects content or enforcement power.').

omega_variable(
    security_threat_magnitude,
    'What is the actual magnitude of the national security threat the override answers, relative to the override''s scope?',
    'Declassified threat assessments; comparison of prosecution composition (expression, association, publication versus violent acts); evidence of foreign interference actually disclosed in proceedings.',
    'A marginal threat converts the override into cover for political control and pushes the structure snare-ward; a substantial threat reattributes part of the measured extraction to the genuine cost of security coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_threat_magnitude, empirical, 'Whether the security override tracks a real threat or manufactures its justification.').

omega_variable(
    judicial_independence_containment,
    'Does the sovereignty override stay contained to national security cases, or does its logic diffuse through the ordinary legal system?',
    'Track non-security judicial review outcomes, government responses to adverse rulings, overseas judge retention, and appointment patterns over the next decade.',
    'Diffusion raises epsilon across the whole legal order and converts the judiciary from a dual-positioned seat to a full target; containment keeps the coordination core bounded and the current classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_containment, empirical, 'Whether the override''s domain expands beyond national security.').

omega_variable(
    exit_class_stratification,
    'Is exit from the constraint available broadly, or stratified by class, skill, and assets?',
    'Migration statistics by occupation and asset class; visa-scheme uptake composition; asset-transfer and remittance flows from the territory.',
    'If exit concentrates in the mobile professional class, measured suppression understates the constraint on the trapped majority and the payer seats'' effective directionality sits nearer the full-target end than aggregate figures suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_class_stratification, empirical, 'Whether the exit option is real for the population or only for its mobile stratum.').

omega_variable(
    term_2047_sunset_status,
    'Does the Basic Law''s fifty-year term make this constraint transitional by declaration, or does the revocability doctrine render the term operative only at the center''s pleasure?',
    'Central government and NPCSC statements on post-2047 arrangements; whether the term is treated as a guarantee (the autonomy-primacy anchor) or as a delegable horizon the center may restructure early.',
    'If the term binds, the constraint carries a declared terminal horizon and transitional-support analysis applies; if revocability supersedes, the constraint is open-ended and the tangled-rope/snare boundary is the live question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_2047_sunset_status, conceptual, 'Whether the 2047 term functions as a sunset or as decoration over revocable delegation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 1997, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_sovereignty_primacy_tr_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 1997, 0.2).
narrative_ontology:measurement(octs_sovereignty_primacy_tr_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2003, 0.24).
narrative_ontology:measurement(octs_sovereignty_primacy_tr_t2010, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(octs_sovereignty_primacy_tr_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2014, 0.34).
narrative_ontology:measurement(octs_sovereignty_primacy_tr_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2019, 0.42).
narrative_ontology:measurement(octs_sovereignty_primacy_tr_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2020, 0.5).
narrative_ontology:measurement(octs_sovereignty_primacy_tr_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2022, 0.56).
narrative_ontology:measurement(octs_sovereignty_primacy_tr_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(octs_sovereignty_primacy_be_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(octs_sovereignty_primacy_be_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement(octs_sovereignty_primacy_be_t2010, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(octs_sovereignty_primacy_be_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(octs_sovereignty_primacy_be_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(octs_sovereignty_primacy_be_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement(octs_sovereignty_primacy_be_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2022, 0.76).
narrative_ontology:measurement(octs_sovereignty_primacy_be_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(octs_sovereignty_primacy_su_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(octs_sovereignty_primacy_su_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2003, 0.36).
narrative_ontology:measurement(octs_sovereignty_primacy_su_t2010, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(octs_sovereignty_primacy_su_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2014, 0.46).
narrative_ontology:measurement(octs_sovereignty_primacy_su_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(octs_sovereignty_primacy_su_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(octs_sovereignty_primacy_su_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2022, 0.84).
narrative_ontology:measurement(octs_sovereignty_primacy_su_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'One Country, Two Systems' covers three structurally distinct claims about the same kernel, decomposed per the epsilon-invariance principle into three stories. This story instantiates the sovereignty-primacy claim (delegated, revocable autonomy; security lexically prior). The autonomy-primacy sibling instantiates the enforceable-guarantee claim; the balanced-coexistence sibling instantiates the negotiated-division claim. Their epsilon values differ widely because the readings assign different victim sets and different enforcement structures to the same territory; the sovereignty-primacy reading's enforcement machinery (annexed security statute, extraterritorial agents, vetting) is what generates this story's high epsilon, and it is precisely what the other two readings deny the center the power to build. Sibling edges are declared both here and in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
